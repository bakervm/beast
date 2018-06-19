use ast::*;
use config::Config;
use defaults;
use failure::ResultExt;
use melon::{typedef::*, Instruction, IntegerType, Register};
use parser::{BeastParser, Rule};
use pest::{iterators::Pair, Parser};
use std::{collections::{BTreeMap, BTreeSet},
          fs::File,
          io::Read,
          path::PathBuf,
          sync::mpsc::{self, TryRecvError},
          thread};

const SOURCE_FILE_EXTENSIONS: [&str; 2] = ["beast", "bst"];

#[derive(Clone)]
pub struct AstGen {
    config: Config,
    include: Vec<String>,
}

impl AstGen {
    fn new(config: Config) -> AstGen {
        let mut include = config.compilation.include_dirs.clone();
        include.push(defaults::INCLUDE_PATH.into());

        AstGen {
            config: config,
            include: include,
        }
    }

    pub fn gen(root_module: String, config: Config) -> Result<Ast> {
        let mut compiler = AstGen::new(config);
        let ast = compiler.ast(root_module)?;

        Ok(ast)
    }

    fn ast(&mut self, root_module: String) -> Result<Ast> {
        let (module_sender, module_receiver) = mpsc::channel();
        let (instructor_sender, instructor_receiver) = mpsc::channel::<String>();

        instructor_sender.send(root_module.clone())?;

        let compiler = self.clone();
        let instructor_sender = instructor_sender.clone();
        thread::spawn(move || {
            while let Ok(module_name) = instructor_receiver.recv() {
                let mut compiler = compiler.clone();
                let module_sender = module_sender.clone();
                thread::spawn(move || {
                    let module = compiler.module(module_name.clone());

                    module_sender.send((module_name, module)).unwrap();
                });
            }
        });

        let mut modules = BTreeMap::new();
        let mut requested_modules = BTreeSet::new();
        requested_modules.insert(root_module);

        loop {
            match module_receiver.try_recv() {
                Ok((module_name, module_res)) => {
                    let module = module_res.with_context(|e| {
                        format!("failed to compile module {:?}\n{}", module_name, e)
                    })?;

                    modules.insert(module_name, module.clone());

                    for import in module.imports {
                        if !requested_modules.contains(&import.module_id) {
                            requested_modules.insert(import.module_id.clone());

                            instructor_sender.send(import.module_id)?;
                        }
                    }
                }
                Err(TryRecvError::Empty) => {
                    if modules.len() == requested_modules.len() {
                        break;
                    }

                    thread::yield_now();
                }
                _ => bail!("an unknown error occured"),
            }
        }

        Ok(Ast { modules: modules })
    }

    fn module(&mut self, module_id: String) -> Result<Module> {
        let module_file = self.discover_module(module_id.clone())?;

        let mut file = File::open(module_file)?;

        let mut buf = String::new();

        file.read_to_string(&mut buf)?;

        let parsing_result = BeastParser::parse(Rule::file, &buf);

        let parsed_file = match parsing_result {
            Err(err) => bail!("{}", err),
            Ok(res) => res,
        };

        let mut imports = Vec::new();
        let mut exports = Vec::new();
        let mut constants = Vec::new();
        let mut funcs = Vec::new();

        for pair in parsed_file {
            match pair.as_rule() {
                Rule::import => {
                    let import = self.import(pair)?;
                    imports.push(import);
                }
                Rule::func => {
                    let func = self.func(pair)?;
                    funcs.push(func);
                }
                Rule::export => {
                    let export = self.export(pair)?;
                    exports.push(export);
                }
                Rule::constant => {
                    let constant = self.constant(pair)?;
                    constants.push(constant);
                }
                _ => unreachable!(),
            }
        }

        Ok(Module {
            id: module_id,
            imports,
            exports,
            constants,
            funcs,
        })
    }

    fn import(&mut self, pair: Pair<Rule>) -> Result<Import> {
        let mut pairs = pair.into_inner();

        let func_name = pairs.next().unwrap().as_str();

        let after_func = pairs.next().unwrap();

        let (func_alias, module_id) = if after_func.as_rule() == Rule::func_alias {
            (Some(after_func.as_str()), pairs.next().unwrap().as_str())
        } else {
            (None, after_func.as_str())
        };

        Ok(Import {
            func_origin_id: func_name.into(),
            func_alias_id: func_alias.unwrap_or(func_name).into(),
            module_id: module_id.into(),
        })
    }

    fn func(&mut self, pair: Pair<Rule>) -> Result<Func> {
        let mut pairs = pair.into_inner();

        let func_name = pairs.next().unwrap().as_str();

        let mut instr_vec = Vec::new();

        for expr in pairs {
            let expr = self.expr(expr)?;

            instr_vec.push(expr);
        }

        Ok(Func {
            id: func_name.into(),
            expr: instr_vec,
        })
    }

    fn constant(&mut self, pair: Pair<Rule>) -> Result<Const> {
        let mut pairs = pair.into_inner();

        let const_id = pairs.next().unwrap().as_str();

        let raw_const_lit = pairs.next().unwrap().as_str();

        Ok(Const {
            id: const_id.into(),
            value: raw_const_lit
                .parse()
                .or_else(|_| i32::from_str_radix(&raw_const_lit[2..], 16))?,
        })
    }

    fn export(&mut self, pair: Pair<Rule>) -> Result<Export> {
        let mut pairs = pair.into_inner();

        let exported_func = pairs.next().unwrap().as_str();

        let alias = pairs.next().and_then(|e| Some(e.as_str())).or_else(|| None);

        Ok(Export {
            func_origin_id: exported_func.into(),
            func_alias_id: alias.unwrap_or(exported_func).into(),
        })
    }

    fn expr(&mut self, pair: Pair<Rule>) -> Result<Expr> {
        let mut pairs = pair.into_inner();

        let plain_instr = pairs.next().unwrap();
        let mut inner = plain_instr.clone().into_inner();

        match plain_instr.as_rule() {
            Rule::push_instr => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);

                let raw_arg = inner.next().unwrap();
                match raw_arg.as_rule() {
                    Rule::constant_id => {
                        let arg = raw_arg.as_str();
                        let inst = match real_type {
                            IntegerType::U8 => Expr::PushConstU8(Argument::Constant(arg.into())),
                            IntegerType::U16 => Expr::PushConstU16(Argument::Constant(arg.into())),
                            IntegerType::I8 => Expr::PushConstI8(Argument::Constant(arg.into())),
                            IntegerType::I16 => Expr::PushConstI16(Argument::Constant(arg.into())),
                        };

                        Ok(inst)
                    }
                    Rule::literal => {
                        let arg = raw_arg.as_str();
                        let inst = match real_type {
                            IntegerType::U8 => Expr::PushConstU8(
                                Argument::Literal(arg.parse().or_else(|_| u8::from_str_radix(&arg[2..], 16))?),
                            ),
                            IntegerType::U16 => Expr::PushConstU16(
                                Argument::Literal(arg.parse().or_else(|_| u16::from_str_radix(&arg[2..], 16))?),
                            ),
                            IntegerType::I8 => Expr::PushConstI8(
                                Argument::Literal(arg.parse().or_else(|_| i8::from_str_radix(&arg[2..], 16))?),
                            ),
                            IntegerType::I16 => Expr::PushConstI16(
                                Argument::Literal(arg.parse().or_else(|_| i16::from_str_radix(&arg[2..], 16))?),
                            ),
                        };

                        Ok(inst)
                    }
                    _ => unreachable!(),
                }
            }
            Rule::add => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Add(real_type)))
            }
            Rule::sub => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Sub(real_type)))
            }
            Rule::mul => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Mul(real_type)))
            }
            Rule::div => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Div(real_type)))
            }
            Rule::shr => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Shr(real_type)))
            }
            Rule::shl => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Shl(real_type)))
            }
            Rule::and => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::And(real_type)))
            }
            Rule::or => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Or(real_type)))
            }
            Rule::xor => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Xor(real_type)))
            }
            Rule::not => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Not(real_type)))
            }
            Rule::neg => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Neg(real_type)))
            }
            Rule::inc => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Inc(real_type)))
            }
            Rule::dec => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Dec(real_type)))
            }
            Rule::u8_promote => Ok(Expr::ActualInstr(Instruction::U8Promote)),
            Rule::u16_demote => Ok(Expr::ActualInstr(Instruction::U16Demote)),
            Rule::i8_promote => Ok(Expr::ActualInstr(Instruction::I8Promote)),
            Rule::i16_demote => Ok(Expr::ActualInstr(Instruction::I16Demote)),
            Rule::reg => {
                let raw_register = inner.next().unwrap().as_str();

                Ok(Expr::ActualInstr(Instruction::LoadReg(
                    self.register(raw_register)?,
                )))
            }
            Rule::load => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);

                if let Some(raw_arg) = inner.next() {
                    let arg = if raw_arg.as_rule() == Rule::constant_id {
                        Argument::Constant(raw_arg.as_str().into())
                    } else {
                        let raw_arg = raw_arg.as_str();
                        Argument::Literal(raw_arg
                            .parse()
                            .or_else(|_| u16::from_str_radix(&raw_arg[2..], 16))?)
                    };

                    Ok(Expr::Load(real_type, arg))
                } else {
                    Ok(Expr::ActualInstr(Instruction::LoadIndirect(real_type)))
                }
            }
            Rule::store => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);

                if let Some(raw_arg) = inner.next() {
                    let arg = if raw_arg.as_rule() == Rule::constant_id {
                        Argument::Constant(raw_arg.as_str().into())
                    } else {
                        let raw_arg = raw_arg.as_str();
                        Argument::Literal(raw_arg
                            .parse()
                            .or_else(|_| u16::from_str_radix(&raw_arg[2..], 16))?)
                    };

                    Ok(Expr::Store(real_type, arg))
                } else {
                    Ok(Expr::ActualInstr(Instruction::StoreIndirect(real_type)))
                }
            }
            Rule::dup => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Dup(real_type)))
            }
            Rule::drop => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = self.type_(raw_type);
                Ok(Expr::ActualInstr(Instruction::Drop(real_type)))
            }
            Rule::sys => {
                let signal = inner.next().unwrap().as_str();
                Ok(Expr::Sys(signal.into()))
            }
            Rule::call => {
                let func_id = inner.next().unwrap().as_str();
                Ok(Expr::Call(func_id.into()))
            }
            Rule::ret => Ok(Expr::ActualInstr(Instruction::Ret)),
            Rule::alloc => {
                let raw_num_const = inner.next().unwrap();

                let arg = if raw_num_const.as_rule() == Rule::constant_id {
                    Argument::Constant(raw_num_const.as_str().into())
                } else {
                    let raw_arg = raw_num_const.as_str();

                    Argument::Literal(raw_arg
                        .parse()
                        .or_else(|_| u16::from_str_radix(&raw_arg[2..], 16))?)
                };

                Ok(Expr::Alloc(arg))
            }
            Rule::free => Ok(Expr::ActualInstr(Instruction::Free)),
            Rule::while_loop => {
                let cond = inner.next().unwrap();

                let condition = match cond.as_rule() {
                    Rule::greater => Condition::Greater,
                    Rule::less => Condition::Less,
                    Rule::greater_equal => Condition::GreaterOrEqual,
                    Rule::less_equal => Condition::LessOrEqual,
                    Rule::equal => Condition::Equal,
                    Rule::unequal => Condition::NotEqual,
                    _ => unreachable!(),
                };

                let type_t = inner.next().unwrap().as_str();
                let real_type = self.type_(type_t);

                let mut instr_vec = Vec::new();

                for expr in inner {
                    let expr = self.expr(expr)?;

                    instr_vec.push(expr);
                }

                Ok(Expr::While(While {
                    cond: condition,
                    type_t: real_type,
                    exprs: instr_vec,
                }))
            }
            Rule::if_cond => {
                let cond = inner.next().unwrap();

                let condition = match cond.as_rule() {
                    Rule::greater => Condition::Greater,
                    Rule::less => Condition::Less,
                    Rule::greater_equal => Condition::GreaterOrEqual,
                    Rule::less_equal => Condition::LessOrEqual,
                    Rule::equal => Condition::Equal,
                    Rule::unequal => Condition::NotEqual,
                    _ => unreachable!(),
                };

                let type_t = inner.next().unwrap().as_str();
                let real_type = self.type_(type_t);

                let mut instr_vec = Vec::new();

                let mut else_branch = None;

                for expr in inner {
                    if expr.as_rule() == Rule::else_cond {
                        let mut else_instr_vec = Vec::new();

                        for expr in expr.into_inner() {
                            let expr = self.expr(expr)?;

                            else_instr_vec.push(expr);
                        }

                        else_branch = Some(else_instr_vec);
                        break;
                    }

                    let expr = self.expr(expr)?;

                    instr_vec.push(expr);
                }

                Ok(Expr::If(If {
                    cond: condition,
                    type_t: real_type,
                    exprs: instr_vec,
                    else_exprs: else_branch,
                }))
            }
            _ => unreachable!(),
        }
    }

    fn type_(&mut self, raw: &str) -> IntegerType {
        match raw {
            "u8" => IntegerType::U8,
            "u16" => IntegerType::U16,
            "i8" => IntegerType::I8,
            "i16" => IntegerType::I16,
            _ => unreachable!(),
        }
    }

    fn register(&mut self, raw: &str) -> Result<Register> {
        let res = match raw {
            ":sp" => Register::StackPtr,
            ":bp" => Register::BasePtr,
            _ => unreachable!(),
        };

        Ok(res)
    }

    fn discover_module(&mut self, module: String) -> Result<PathBuf> {
        let orig_module = module.clone();

        let base_path: PathBuf = orig_module.split('.').collect();

        let beast_module_name = base_path.with_extension(SOURCE_FILE_EXTENSIONS[0]);

        let bst_module_name = base_path.with_extension(SOURCE_FILE_EXTENSIONS[1]);

        let found_module = self.include
            .iter()
            .map(|include| PathBuf::from(include).join(beast_module_name.clone()))
            .chain(
                self.include
                    .iter()
                    .map(|include| PathBuf::from(include).join(bst_module_name.clone())),
            )
            .find(|include| include.exists());

        if let Some(module_id) = found_module {
            return Ok(module_id);
        }

        bail!("unable to find module: {:?}", module)
    }
}
