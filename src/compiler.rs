use ast::*;
use config::Config;
use melon::typedef::*;
use melon::{IntegerType, Program, Register};
use parser::{BeastParser, Rule};
use pest::Parser;
use pest::iterators::Pair;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};

const BEAST_FILE_EXTENSION: &str = "beast";

pub struct Compiler {
    instructions: Vec<Instruction>,
    config: Config,
}

impl Compiler {
    pub fn compile<P: AsRef<Path>>(path: P, config: Config) -> Result<Program> {
        let root_file_path = path.as_ref().to_path_buf().canonicalize()?;

        let ast = Compiler::compile_ast(root_file_path)?;

        println!("{:#?}", ast);

        Ok(Program {
            target_version: "".into(),
            system_id: "".into(),
            instructions: Vec::new(),
            mem_pages: None,
        })
    }

    fn compile_ast(root_path: PathBuf) -> Result<Ast> {
        let mut ast = Ast {
            modules: Vec::new(),
        };

        let module = Compiler::compile_file(root_path)?;

        ast.modules.push(module);

        Ok(ast)
    }

    fn compile_file(path: PathBuf) -> Result<Module> {
        let mut file = File::open(path)?;

        let mut buf = String::new();

        file.read_to_string(&mut buf)?;

        let parsing_result = BeastParser::parse(Rule::file, &buf);

        if let Err(err) = parsing_result {
            bail!("{}", err);
        }

        let parsed_file = parsing_result.unwrap();

        let mut module = Module {
            imports: Vec::new(),
            exports: Vec::new(),
            constants: Vec::new(),
            funcs: Vec::new(),
        };

        for pair in parsed_file {
            match pair.as_rule() {
                Rule::import => {
                    let import = Compiler::compile_import(pair)?;
                    module.imports.push(import);
                }
                Rule::func => {
                    let func = Compiler::compile_func(pair)?;
                    module.funcs.push(func);
                }
                Rule::export => {
                    let export = Compiler::compile_export(pair)?;
                    module.exports.push(export);
                }
                Rule::constant => {
                    let constant = Compiler::compile_constant(pair)?;
                    module.constants.push(constant);
                }
                _ => unreachable!(),
            }
        }

        Ok(module)
    }

    fn compile_import(pair: Pair<Rule>) -> Result<Import> {
        let mut pairs = pair.into_inner();

        let func_name = pairs.next().unwrap().as_str();

        let after_func = pairs.next().unwrap();

        let (func_alias, module_path) = if after_func.as_rule() == Rule::func_alias {
            (Some(after_func.as_str()), pairs.next().unwrap().as_str())
        } else {
            (None, after_func.as_str())
        };

        let mut module_path = module_path.to_owned();
        module_path.pop();
        module_path.remove(0);

        Ok(Import {
            origin_name: func_name.into(),
            alias: func_alias.unwrap_or(func_name).into(),
            module_path: module_path,
        })
    }

    fn compile_func(pair: Pair<Rule>) -> Result<Func> {
        let mut pairs = pair.into_inner();

        let func_name = pairs.next().unwrap().as_str();

        let mut instr_vec = Vec::new();

        for instr in pairs {
            let instr = Compiler::compile_instr(instr)?;

            instr_vec.push(instr);
        }

        Ok(Func {
            name: func_name.into(),
            instr: instr_vec,
        })
    }

    fn compile_constant(pair: Pair<Rule>) -> Result<Const> {
        let mut pairs = pair.into_inner();

        let const_name = pairs.next().unwrap().as_str();

        let raw_const_lit = pairs.next().unwrap().as_str();

        Ok(Const {
            name: const_name.into(),
            value: raw_const_lit.parse()?,
        })
    }

    fn compile_export(pair: Pair<Rule>) -> Result<Export> {
        let mut pairs = pair.into_inner();

        let exported_func = pairs.next().unwrap().as_str();

        let alias = pairs.next().and_then(|e| Some(e.as_str())).or_else(|| None);

        Ok(Export {
            origin_name: exported_func.into(),
            alias: alias.unwrap_or(exported_func).into(),
        })
    }

    fn compile_instr(pair: Pair<Rule>) -> Result<Instruction> {
        let mut pairs = pair.into_inner();

        let plain_instr = pairs.next().unwrap();
        let mut inner = plain_instr.clone().into_inner();

        match plain_instr.as_rule() {
            Rule::push_instr => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);

                let raw_arg = inner.next().unwrap();
                match raw_arg.as_rule() {
                    Rule::constant_id => {
                        let arg = raw_arg.as_str();
                        let inst = match real_type {
                            IntegerType::U8 => {
                                Instruction::PushConstU8(Argument::Constant(arg.into()))
                            }
                            IntegerType::U16 => {
                                Instruction::PushConstU16(Argument::Constant(arg.into()))
                            }
                            IntegerType::I8 => {
                                Instruction::PushConstI8(Argument::Constant(arg.into()))
                            }
                            IntegerType::I16 => {
                                Instruction::PushConstI16(Argument::Constant(arg.into()))
                            }
                        };

                        Ok(inst)
                    }
                    Rule::literal => {
                        let arg = raw_arg.as_str();
                        let inst = match real_type {
                            IntegerType::U8 => Instruction::PushConstU8(Argument::Literal(
                                arg.parse().or_else(|_| u8::from_str_radix(&arg[2..], 16))?,
                            )),
                            IntegerType::U16 => Instruction::PushConstU16(Argument::Literal(
                                arg.parse().or_else(|_| u16::from_str_radix(&arg[2..], 16))?,
                            )),
                            IntegerType::I8 => Instruction::PushConstI8(Argument::Literal(
                                arg.parse().or_else(|_| i8::from_str_radix(&arg[2..], 16))?,
                            )),
                            IntegerType::I16 => Instruction::PushConstI16(Argument::Literal(
                                arg.parse().or_else(|_| i16::from_str_radix(&arg[2..], 16))?,
                            )),
                        };

                        Ok(inst)
                    }
                    _ => unreachable!(),
                }
            }
            Rule::add => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Add(real_type))
            }
            Rule::sub => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Sub(real_type))
            }
            Rule::mul => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Mul(real_type))
            }
            Rule::div => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Div(real_type))
            }
            Rule::shr => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Shr(real_type))
            }
            Rule::shl => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Shl(real_type))
            }
            Rule::and => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::And(real_type))
            }
            Rule::or => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Or(real_type))
            }
            Rule::xor => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Xor(real_type))
            }
            Rule::not => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Not(real_type))
            }
            Rule::neg => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Neg(real_type))
            }
            Rule::inc => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Inc(real_type))
            }
            Rule::dec => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Dec(real_type))
            }
            Rule::u8_promote => Ok(Instruction::U8Promote),
            Rule::u16_demote => Ok(Instruction::U16Demote),
            Rule::i8_promote => Ok(Instruction::I8Promote),
            Rule::i16_demote => Ok(Instruction::I16Demote),
            Rule::reg => {
                let raw_register = inner.next().unwrap().as_str();

                Ok(Instruction::LoadReg(Compiler::compile_register(
                    raw_register,
                )?))
            }
            Rule::load => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);

                if let Some(raw_arg) = inner.next() {
                    let arg = if raw_arg.as_rule() == Rule::constant_id {
                        Argument::Constant(raw_arg.as_str().into())
                    } else {
                        let raw_arg = raw_arg.as_str();
                        Argument::Literal(raw_arg
                            .parse()
                            .or_else(|_| u16::from_str_radix(&raw_arg[2..], 16))?)
                    };

                    Ok(Instruction::Load(real_type, arg))
                } else {
                    Ok(Instruction::LoadIndirect(real_type))
                }
            }
            Rule::store => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);

                if let Some(raw_arg) = inner.next() {
                    let arg = if raw_arg.as_rule() == Rule::constant_id {
                        Argument::Constant(raw_arg.as_str().into())
                    } else {
                        let raw_arg = raw_arg.as_str();
                        Argument::Literal(raw_arg
                            .parse()
                            .or_else(|_| u16::from_str_radix(&raw_arg[2..], 16))?)
                    };

                    Ok(Instruction::Store(real_type, arg))
                } else {
                    Ok(Instruction::StoreIndirect(real_type))
                }
            }
            Rule::dup => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Dup(real_type))
            }
            Rule::drop => {
                let raw_type = inner.next().unwrap().as_str();
                let real_type = Compiler::compile_type(raw_type);
                Ok(Instruction::Drop(real_type))
            }
            Rule::sys => {
                let signal = inner.next().unwrap().as_str();
                Ok(Instruction::Sys(signal.into()))
            }
            Rule::call => Ok(Instruction::U8Promote),
            Rule::ret => Ok(Instruction::Ret),
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

                Ok(Instruction::Alloc(arg))
            }
            Rule::free => Ok(Instruction::Free),
            Rule::while_loop => {
                let cond = inner.next().unwrap();

                let condition = match cond.as_rule() {
                    Rule::positive => IfCond::Positive,
                    Rule::negative => IfCond::Negative,
                    Rule::zero => IfCond::Zero,
                    Rule::not_zero => IfCond::NotZero,
                    _ => unreachable!(),
                };

                let mut instr_vec = Vec::new();

                for instr in inner {
                    let instr = Compiler::compile_instr(instr)?;

                    instr_vec.push(instr);
                }

                Ok(Instruction::While(While(condition, instr_vec)))
            }
            Rule::if_cond => {
                let cond = inner.next().unwrap();

                let condition = match cond.as_rule() {
                    Rule::positive => IfCond::Positive,
                    Rule::negative => IfCond::Negative,
                    Rule::zero => IfCond::Zero,
                    Rule::not_zero => IfCond::NotZero,
                    _ => unreachable!(),
                };

                let mut instr_vec = Vec::new();

                let mut else_branch = None;

                for instr in inner {
                    if instr.as_rule() == Rule::else_cond {
                        let mut else_instr_vec = Vec::new();

                        for instr in instr.into_inner() {
                            let instr = Compiler::compile_instr(instr)?;

                            else_instr_vec.push(instr);
                        }

                        else_branch = Some(else_instr_vec);
                        break;
                    }

                    let instr = Compiler::compile_instr(instr)?;

                    instr_vec.push(instr);
                }

                Ok(Instruction::If(If(condition, instr_vec, else_branch)))
            }
            _ => unreachable!(),
        }
    }

    fn compile_type(raw: &str) -> IntegerType {
        match raw {
            "u8" => IntegerType::U8,
            "u16" => IntegerType::U16,
            "i8" => IntegerType::I8,
            "i16" => IntegerType::I16,
            _ => unreachable!(),
        }
    }

    fn compile_register(raw: &str) -> Result<Register> {
        let res = match raw {
            ":sp" => Register::StackPtr,
            ":bp" => Register::BasePtr,
            reg => bail!("unrecognized register identifier: {}", reg),
        };

        Ok(res)
    }
}
