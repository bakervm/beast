use ast::*;
use ast_gen::AstGen;
use defaults;
use melon::{typedef::*, Instruction, Program, ProgramBuilder};
use std::collections::BTreeMap;
use std::{path::PathBuf, str::FromStr};

const PRIVATE_PREFIX: &str = "PRIVATE__";

pub struct SignalPair {
    pub key: String,
    pub value: u16,
}

impl FromStr for SignalPair {
    type Err = ::failure::Error;

    fn from_str(s: &str) -> std::result::Result<SignalPair, Self::Err> {
        let main_split: Vec<_> = s.split('=').map(|e| e.trim()).collect();

        ensure!(
            main_split.len() == 2,
            "invalid key-value pair. Expected 'identifier=number'"
        );

        Ok(SignalPair {
            key: main_split[0].into(),
            value: main_split[1].parse()?,
        })
    }
}

#[derive(Debug)]
enum MetaInstr {
    ActualInstr(Instruction),
    Call { func_id: String, module_id: String },
}

pub struct Compiler {
    ast: Ast,
    signals: Vec<SignalPair>,
}

impl Compiler {
    fn new(ast: Ast, signals: Vec<SignalPair>) -> Compiler {
        Compiler { ast, signals }
    }

    pub fn compile(
        root_dir: PathBuf,
        module: String,
        system_id: String,
        mem_pages: Option<u8>,
        signals: Vec<SignalPair>,
        include: Vec<PathBuf>,
    ) -> Result<Program> {
        let mut include = include;
        include.push(root_dir);

        let ast = AstGen::gen(include, module)?;

        let program = Compiler::new(ast, signals).build(system_id, mem_pages)?;

        Ok(program)
    }

    fn build(&mut self, system_id: String, mem_pages: Option<u8>) -> Result<Program> {
        let modules = self.ast.modules.clone();

        let mut meta_module_map = BTreeMap::new();

        for (module_name, module) in modules {
            let mut meta_func_map = BTreeMap::new();

            for func in &module.funcs {
                let mut meta_instr = self.to_meta_instr(func.expr.clone(), &module)?;

                let exported_func = module
                    .exports
                    .iter()
                    .find(|exp| exp.func_origin_id == func.id);

                let mut func_id = exported_func
                    .map(|exp| exp.func_alias_id.clone())
                    .unwrap_or_else(|| format!("{}{}", PRIVATE_PREFIX, func.id));

                if module_name == self.ast.root_module && func.id == defaults::ENTRY_POINT_FUNC {
                    func_id = defaults::ENTRY_POINT_FUNC.into();
                    meta_instr.push(MetaInstr::ActualInstr(Instruction::SysCall(0)));
                } else {
                    meta_instr.push(MetaInstr::ActualInstr(Instruction::Ret));
                }

                meta_func_map.insert(func_id, meta_instr);
            }

            meta_module_map.insert(module_name, meta_func_map);
        }

        let mut meta_instr_vec = Vec::new();
        let mut module_map = BTreeMap::new();

        for (meta_module_name, outer_func_map) in meta_module_map {
            let final_func_map = {
                let mut func_map = BTreeMap::new();

                for (meta_func_id, mut meta_func) in outer_func_map {
                    let offset = meta_instr_vec.len();

                    func_map.insert(meta_func_id, offset);

                    meta_instr_vec.append(&mut meta_func);
                }

                func_map
            };

            module_map.insert(meta_module_name, final_func_map);
        }

        ensure!(
            meta_instr_vec.len() <= (u16::max_value() as usize),
            "program has too many instructions ({}). Maximum number of instructions: {}",
            meta_instr_vec.len(),
            u16::max_value()
        );

        let mut final_instructions = Vec::new();
        for meta_instr in meta_instr_vec {
            let instr = match meta_instr {
                MetaInstr::ActualInstr(instr) => instr,
                MetaInstr::Call { func_id, module_id } => {
                    let func_map = module_map
                        .get(&module_id)
                        .ok_or_else(|| format_err!("unable to find module {:?}", module_id))?;

                    let func_addr = func_map
                        .get(&func_id)
                        .ok_or_else(|| format_err!("unable to find function {:?}", func_id))?;

                    Instruction::Call(*func_addr as u16)
                }
            };

            final_instructions.push(instr);
        }

        let entry_func_map = module_map.get(&self.ast.root_module).ok_or_else(|| {
            format_err!("unable to find entry module {:?}", &self.ast.root_module)
        })?;

        let entry_func_addr = entry_func_map
            .get(defaults::ENTRY_POINT_FUNC)
            .ok_or_else(|| {
                format_err!(
                    "unable to find entry function {:?}",
                    defaults::ENTRY_POINT_FUNC
                )
            })?;

        let program_builder = ProgramBuilder::new(system_id)
            .instructions(final_instructions)
            .entry_point(*entry_func_addr as u16);

        let program = if let Some(pages) = mem_pages {
            program_builder.mem_pages(pages).gen()
        } else {
            program_builder.gen()
        };

        Ok(program)
    }

    fn to_meta_instr(&self, instrs: Vec<Expr>, module: &Module) -> Result<Vec<MetaInstr>> {
        let mut meta_vec = Vec::new();

        for instr in instrs {
            match instr.clone() {
                Expr::While(While {
                    cond,
                    type_t,
                    exprs,
                }) => {
                    meta_vec.push(MetaInstr::ActualInstr(Instruction::Cmp(type_t)));

                    let mut meta_instrs = self.to_meta_instr(exprs, module)?;

                    let meta_len = meta_instrs.len() as u16;

                    meta_vec.push(MetaInstr::ActualInstr(match cond {
                        Condition::Greater => Instruction::JltEq(true, meta_len + 2),
                        Condition::Less => Instruction::JgtEq(true, meta_len + 2),
                        Condition::Equal => Instruction::Jneq(true, meta_len + 2),
                        Condition::NotEqual => Instruction::Jeq(true, meta_len + 2),
                        Condition::GreaterOrEqual => Instruction::Jlt(true, meta_len + 2),
                        Condition::LessOrEqual => Instruction::Jgt(true, meta_len + 2),
                    }));

                    meta_vec.append(&mut meta_instrs);
                    meta_vec.push(MetaInstr::ActualInstr(Instruction::Jmp(
                        false,
                        meta_len + 2,
                    )));
                    continue;
                }
                Expr::If(If {
                    cond,
                    type_t,
                    exprs,
                    else_exprs,
                }) => {
                    meta_vec.push(MetaInstr::ActualInstr(Instruction::Cmp(type_t)));

                    let mut if_meta_instrs = self.to_meta_instr(exprs, module)?;

                    let mut else_meta_instrs = Vec::new();

                    if let Some(instrs) = else_exprs {
                        else_meta_instrs = self.to_meta_instr(instrs, module)?;

                        let else_meta_len = else_meta_instrs.len() as u16;

                        if_meta_instrs.push(MetaInstr::ActualInstr(Instruction::Jmp(
                            true,
                            else_meta_len + 1,
                        )));
                    }

                    let if_meta_len = if_meta_instrs.len() as u16;

                    meta_vec.push(MetaInstr::ActualInstr(match cond {
                        Condition::Greater => Instruction::JltEq(true, if_meta_len + 1),
                        Condition::Less => Instruction::JgtEq(true, if_meta_len + 1),
                        Condition::Equal => Instruction::Jneq(true, if_meta_len + 1),
                        Condition::NotEqual => Instruction::Jeq(true, if_meta_len + 1),
                        Condition::GreaterOrEqual => Instruction::Jlt(true, if_meta_len + 1),
                        Condition::LessOrEqual => Instruction::Jgt(true, if_meta_len + 1),
                    }));

                    meta_vec.append(&mut if_meta_instrs);
                    meta_vec.append(&mut else_meta_instrs);
                    continue;
                }
                _ => {}
            }

            let meta_instr = match instr {
                Expr::PushConstU8(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, &id)?;

                        MetaInstr::ActualInstr(Instruction::PushConstU8(value as u8))
                    }
                    Argument::Literal(lit) => MetaInstr::ActualInstr(Instruction::PushConstU8(lit)),
                },
                Expr::PushConstU16(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, &id)?;

                        MetaInstr::ActualInstr(Instruction::PushConstU16(value as u16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(Instruction::PushConstU16(lit))
                    }
                },
                Expr::PushConstI8(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, &id)?;

                        MetaInstr::ActualInstr(Instruction::PushConstI8(value as i8))
                    }
                    Argument::Literal(lit) => MetaInstr::ActualInstr(Instruction::PushConstI8(lit)),
                },
                Expr::PushConstI16(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, &id)?;

                        MetaInstr::ActualInstr(Instruction::PushConstI16(value as i16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(Instruction::PushConstI16(lit))
                    }
                },
                Expr::Load(integer_type, arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, &id)?;

                        MetaInstr::ActualInstr(Instruction::Load(integer_type, value as u16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(Instruction::Load(integer_type, lit))
                    }
                },
                Expr::Store(integer_type, arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, &id)?;

                        MetaInstr::ActualInstr(Instruction::Store(integer_type, value as u16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(Instruction::Store(integer_type, lit))
                    }
                },
                Expr::Sys(mut signal) => {
                    signal.remove(0);

                    let real_signal = if signal == "halt" {
                        0
                    } else {
                        ensure!(!self.signals.is_empty(), "no signals available");

                        self.signals
                            .iter()
                            .find(|item| item.key == signal)
                            .map(|pair| pair.value)
                            .ok_or_else(|| {
                                format_err!(
                                    "undefined signal {:?}. Available signals are {:?}",
                                    signal,
                                    self.signals
                                        .iter()
                                        .map(|e| e.key.clone())
                                        .collect::<Vec<_>>()
                                )
                            })?
                    };

                    MetaInstr::ActualInstr(Instruction::SysCall(real_signal))
                }
                Expr::Call(func_id) => {
                    let opt_import = module
                        .imports
                        .iter()
                        .find(|import| import.func_alias_id == func_id);

                    if let Some(ref import) = opt_import {
                        MetaInstr::Call {
                            func_id: import.func_origin_id.clone(),
                            module_id: import.module_id.clone(),
                        }
                    } else {
                        let opt_local = module
                            .funcs
                            .iter()
                            .find(|other_func| other_func.id == func_id);

                        if let Some(ref local) = opt_local {
                            let exported_func = module
                                .exports
                                .iter()
                                .find(|exp| exp.func_origin_id == local.id);

                            let func_id = exported_func
                                .map(|exp| exp.func_alias_id.clone())
                                .unwrap_or_else(|| format!("{}{}", PRIVATE_PREFIX, local.id));

                            MetaInstr::Call {
                                func_id,
                                module_id: module.id.clone(),
                            }
                        } else {
                            bail!(
                                "unable to find function definition or import for {:?}",
                                func_id
                            );
                        }
                    }
                }
                Expr::Alloc(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, &id)?;

                        MetaInstr::ActualInstr(Instruction::Alloc(value as u16))
                    }
                    Argument::Literal(lit) => MetaInstr::ActualInstr(Instruction::Alloc(lit)),
                },
                Expr::ActualInstr(instr) => MetaInstr::ActualInstr(instr),
                _ => unreachable!(),
            };

            meta_vec.push(meta_instr);
        }

        Ok(meta_vec)
    }

    fn find_const(consts: &[Const], id: &str) -> Result<i32> {
        let cons = consts
            .iter()
            .find(|con| con.id == id)
            .ok_or_else(|| format_err!("unable to find constant: {:?}", id))?;

        Ok(cons.value)
    }
}
