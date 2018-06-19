use ast::*;
use ast_gen::AstGen;
use config::Config;
use defaults;
use melon::{typedef::*, Instruction, Program};
use std::collections::BTreeMap;

const PRIVATE_PREFIX: &str = "PRIVATE__";

#[derive(Debug)]
enum MetaInstr {
    ActualInstr(Instruction),
    Call { func_id: String, module_id: String },
}

pub struct Compiler {
    ast: Ast,
    config: Config,
}

impl Compiler {
    fn new(config: Config, ast: Ast) -> Compiler {
        Compiler { ast, config }
    }

    pub fn compile(
        root_module: String,
        config: Config,
        emit_func_map: bool,
        emit_ast: bool,
    ) -> Result<Program> {
        let ast = AstGen::gen(root_module.clone(), config.clone())?;

        if emit_ast {
            println!("{:#?}", ast);
        }

        let mut compiler = Compiler::new(config, ast);
        let program = compiler.build(root_module, emit_func_map)?;

        Ok(program)
    }

    fn build(&mut self, root_module: String, emit_func_map: bool) -> Result<Program> {
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

                let mut func_id = if let Some(exp) = exported_func {
                    exp.func_alias_id.clone()
                } else {
                    format!("{}{}", PRIVATE_PREFIX, func.id)
                };

                if module_name == root_module && func.id == defaults::ENTRY_POINT_FUNC {
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

        if emit_func_map {
            println!("{:#?}", module_map);
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
                        .ok_or(format_err!("unable to find module {:?}", module_id))?;

                    let func_addr = func_map
                        .get(&func_id)
                        .ok_or(format_err!("unable to find function {:?}", func_id))?;

                    Instruction::Call(*func_addr as u16)
                }
            };

            final_instructions.push(instr);
        }

        let entry_func_map = module_map
            .get(defaults::BIN_ENTRY_POINT_MODULE)
            .ok_or(format_err!(
                "unable to find entry module {:?}",
                defaults::BIN_ENTRY_POINT_MODULE
            ))?;

        let entry_func_addr = entry_func_map
            .get(defaults::ENTRY_POINT_FUNC)
            .ok_or(format_err!(
                "unable to find entry function {:?}",
                defaults::ENTRY_POINT_FUNC
            ))?;

        Ok(Program {
            target_version: self.config.program.target_version.clone(),
            system_id: self.config.program.system_id.clone(),
            instructions: final_instructions,
            mem_pages: self.config.program.mem_pages.clone(),
            entry_point: *entry_func_addr as u16,
        })
    }

    fn to_meta_instr(&mut self, instrs: Vec<Expr>, module: &Module) -> Result<Vec<MetaInstr>> {
        let mut meta_vec = Vec::new();

        for instr in instrs {
            match instr.clone() {
                Expr::While(whl) => {
                    let While {
                        cond,
                        type_t,
                        exprs,
                    } = whl.clone();

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
                Expr::If(whether) => {
                    let If {
                        cond,
                        type_t,
                        exprs,
                        else_exprs,
                    } = whether.clone();

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
                        let value = Compiler::find_const(&module.constants, id)?;

                        MetaInstr::ActualInstr(Instruction::PushConstU8(value as u8))
                    }
                    Argument::Literal(lit) => MetaInstr::ActualInstr(Instruction::PushConstU8(lit)),
                },
                Expr::PushConstU16(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, id)?;

                        MetaInstr::ActualInstr(Instruction::PushConstU16(value as u16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(Instruction::PushConstU16(lit))
                    }
                },
                Expr::PushConstI8(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, id)?;

                        MetaInstr::ActualInstr(Instruction::PushConstI8(value as i8))
                    }
                    Argument::Literal(lit) => MetaInstr::ActualInstr(Instruction::PushConstI8(lit)),
                },
                Expr::PushConstI16(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, id)?;

                        MetaInstr::ActualInstr(Instruction::PushConstI16(value as i16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(Instruction::PushConstI16(lit))
                    }
                },
                Expr::Load(integer_type, arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, id)?;

                        MetaInstr::ActualInstr(Instruction::Load(integer_type, value as u16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(Instruction::Load(integer_type, lit))
                    }
                },
                Expr::Store(integer_type, arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&module.constants, id)?;

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
                        ensure!(self.config.signals.len() > 0, "no signals were given");

                        *self.config.signals.get(&signal).ok_or(format_err!(
                            "undefined signal {:?}. Available signals are {:?}",
                            signal,
                            self.config.signals.keys().cloned().collect::<Vec<_>>()
                        ))?
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

                            let func_id = if let Some(exp) = exported_func {
                                exp.func_alias_id.clone()
                            } else {
                                format!("{}{}", PRIVATE_PREFIX, local.id)
                            };

                            MetaInstr::Call {
                                func_id: func_id,
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
                        let value = Compiler::find_const(&module.constants, id)?;

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

    fn find_const(consts: &Vec<Const>, id: String) -> Result<i32> {
        let cons = consts
            .iter()
            .find(|con| con.id == id)
            .ok_or(format_err!("unable to find constant: {:?}", id))?;

        Ok(cons.value)
    }
}
