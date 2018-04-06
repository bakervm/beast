use ast::{
    Argument, Ast, Const, Export, Func, If, IfCond, Import, Instruction as BeastInstruction,
    Module, While,
};
use ast_gen::{self, AstGen};
use config::Config;
use library::Lib;
use melon::{Instruction as MelonInstruction, Program, typedef::*};
use std::collections::BTreeMap;

const PRIVATE_PREFIX: &str = "PRIVATE__";

#[derive(Debug)]
enum MetaInstr {
    ActualInstr(MelonInstruction),
    Call {
        this_func: String,
        in_this_module: String,
    },
}

#[derive(Debug)]
enum FuncMapOrLib {
    FuncMap(BTreeMap<String, Vec<MetaInstr>>),
    Lib(Lib),
}

pub struct Compiler {
    ast: Ast,
    config: Config,
}

impl Compiler {
    fn new(config: Config, ast: Ast) -> Compiler {
        Compiler { ast, config }
    }

    pub fn compile(root_module: String, config: Config) -> Result<Program> {
        let ast = AstGen::gen(root_module.clone(), config.clone())?;

        let mut compiler = Compiler::new(config, ast);
        let program = compiler.build(root_module)?;

        Ok(program)
    }

    fn build(&mut self, root_module: String) -> Result<Program> {
        let modules = self.ast.modules.clone();

        let mut meta_module_map = BTreeMap::new();

        for (module_name, module) in modules {
            let func_map_or_lib = match module {
                Module::Lib(lib) => FuncMapOrLib::Lib(lib),
                Module::Source {
                    funcs,
                    imports,
                    exports,
                    path,
                    constants,
                } => {
                    let mut meta_func_map = BTreeMap::new();

                    for func in &funcs {
                        let mut meta_instr = self.to_meta_instr(
                            func.instr.clone(),
                            &exports,
                            &constants,
                            &imports,
                            &funcs,
                            &path,
                        )?;

                        meta_instr.push(MetaInstr::ActualInstr(MelonInstruction::Ret));

                        let exported_func = exports.iter().find(|exp| exp.origin_name == func.name);

                        let mut func_name = if let Some(exp) = exported_func {
                            exp.alias.clone()
                        } else {
                            format!("{}{}", PRIVATE_PREFIX, func.name)
                        };

                        if module_name == root_module
                            && func.name == ast_gen::BEAST_ENTRY_POINT_FUNC
                        {
                            func_name = ast_gen::BEAST_ENTRY_POINT_FUNC.into();
                        }

                        meta_func_map.insert(func_name, meta_instr);
                    }

                    FuncMapOrLib::FuncMap(meta_func_map)
                }
            };

            meta_module_map.insert(module_name, func_map_or_lib);
        }

        let mut meta_instr_vec = Vec::new();
        let mut module_map = BTreeMap::new();

        for (meta_module_name, func_map_or_lib) in meta_module_map {
            let final_func_map = match func_map_or_lib {
                FuncMapOrLib::Lib(lib) => {
                    let offset = meta_instr_vec.len();
                    let mut func_map = BTreeMap::new();

                    for (func_name, lib_offset) in lib.exports {
                        func_map.insert(func_name, lib_offset + offset);
                    }

                    let mut lib_meta_instr: Vec<_> = lib.instructions
                        .iter()
                        .map(|lib_instr| MetaInstr::ActualInstr(lib_instr.clone()))
                        .collect();

                    meta_instr_vec.append(&mut lib_meta_instr);

                    func_map
                }
                FuncMapOrLib::FuncMap(map) => {
                    let mut func_map = BTreeMap::new();

                    for (meta_func_name, mut meta_func) in map {
                        let offset = meta_instr_vec.len();

                        func_map.insert(meta_func_name, offset);

                        meta_instr_vec.append(&mut meta_func);
                    }

                    func_map
                }
            };

            module_map.insert(meta_module_name, final_func_map);
        }

        ensure!(
            meta_instr_vec.len() <= (UInt::max_value() as usize),
            "program has too many instructions ({}). Maximum number of instructions: {}",
            meta_instr_vec.len(),
            UInt::max_value()
        );

        let mut final_instructions = Vec::new();
        for meta_instr in meta_instr_vec {
            let instr = match meta_instr {
                MetaInstr::ActualInstr(instr) => instr,
                MetaInstr::Call {
                    this_func,
                    in_this_module,
                } => {
                    let func_map = module_map
                        .get(&in_this_module)
                        .ok_or(format_err!("unable to find module {:?}", in_this_module))?;

                    let func_addr = func_map
                        .get(&this_func)
                        .ok_or(format_err!("unable to find function {:?}", this_func))?;

                    MelonInstruction::Call(*func_addr as u16)
                }
            };

            final_instructions.push(instr);
        }

        let entry_func_map = module_map
            .get(ast_gen::BEAST_DEFAULT_ENTRY_POINT_MODULE)
            .ok_or(format_err!(
                "unable to find module {:?}",
                ast_gen::BEAST_DEFAULT_ENTRY_POINT_MODULE
            ))?;

        let entry_func_addr = entry_func_map.get(ast_gen::BEAST_ENTRY_POINT_FUNC).ok_or(
            format_err!(
                "unable to find function {:?}",
                ast_gen::BEAST_ENTRY_POINT_FUNC
            ),
        )?;

        Ok(Program {
            target_version: self.config.program.target_version.clone(),
            system_id: self.config.program.system_id.clone(),
            instructions: final_instructions,
            mem_pages: self.config.program.mem_pages.clone(),
            entry_point: *entry_func_addr as u16,
        })
    }

    fn to_meta_instr(
        &mut self,
        instrs: Vec<BeastInstruction>,
        exports: &Vec<Export>,
        consts: &Vec<Const>,
        imports: &Vec<Import>,
        other_funcs: &Vec<Func>,
        local_module_path: &String,
    ) -> Result<Vec<MetaInstr>> {
        let mut meta_vec = Vec::new();

        for instr in instrs {
            match instr.clone() {
                BeastInstruction::While(whl) => {
                    let While(cond, int_type, while_instrs) = whl.clone();

                    meta_vec.push(MetaInstr::ActualInstr(MelonInstruction::Cmp(int_type)));

                    let mut meta_instrs = self.to_meta_instr(
                        while_instrs,
                        exports,
                        consts,
                        imports,
                        other_funcs,
                        local_module_path,
                    )?;

                    let meta_len = meta_instrs.len() as u16;

                    meta_vec.push(MetaInstr::ActualInstr(match cond {
                        IfCond::Positive => MelonInstruction::Jn(true, meta_len + 2),
                        IfCond::Negative => MelonInstruction::Jp(true, meta_len + 2),
                        IfCond::Zero => MelonInstruction::Jnz(true, meta_len + 2),
                        IfCond::NotZero => MelonInstruction::Jz(true, meta_len + 2),
                    }));

                    meta_vec.append(&mut meta_instrs);
                    meta_vec.push(MetaInstr::ActualInstr(MelonInstruction::Jmp(
                        false,
                        meta_len + 2,
                    )));
                    continue;
                }
                BeastInstruction::If(whether) => {
                    let If(cond, int_type, if_instrs, else_instrs) = whether.clone();

                    meta_vec.push(MetaInstr::ActualInstr(MelonInstruction::Cmp(int_type)));

                    let mut if_meta_instrs = self.to_meta_instr(
                        if_instrs,
                        exports,
                        consts,
                        imports,
                        other_funcs,
                        local_module_path,
                    )?;

                    let mut else_meta_instrs = Vec::new();

                    if let Some(instrs) = else_instrs {
                        else_meta_instrs = self.to_meta_instr(
                            instrs,
                            exports,
                            consts,
                            imports,
                            other_funcs,
                            local_module_path,
                        )?;

                        let else_meta_len = else_meta_instrs.len() as u16;

                        if_meta_instrs.push(MetaInstr::ActualInstr(MelonInstruction::Jmp(
                            true,
                            else_meta_len + 1,
                        )));
                    }

                    let if_meta_len = if_meta_instrs.len() as u16;

                    meta_vec.push(MetaInstr::ActualInstr(match cond {
                        IfCond::Positive => MelonInstruction::Jn(true, if_meta_len + 1),
                        IfCond::Negative => MelonInstruction::Jp(true, if_meta_len + 1),
                        IfCond::Zero => MelonInstruction::Jnz(true, if_meta_len + 1),
                        IfCond::NotZero => MelonInstruction::Jz(true, if_meta_len + 1),
                    }));

                    meta_vec.append(&mut if_meta_instrs);
                    meta_vec.append(&mut else_meta_instrs);
                    continue;
                }
                _ => {}
            }

            let meta_instr = match instr {
                BeastInstruction::Add(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Add(integer_type))
                }
                BeastInstruction::Sub(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Sub(integer_type))
                }
                BeastInstruction::Mul(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Mul(integer_type))
                }
                BeastInstruction::Div(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Div(integer_type))
                }
                BeastInstruction::Shr(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Shr(integer_type))
                }
                BeastInstruction::Shl(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Shl(integer_type))
                }
                BeastInstruction::And(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::And(integer_type))
                }
                BeastInstruction::Or(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Or(integer_type))
                }
                BeastInstruction::Xor(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Xor(integer_type))
                }
                BeastInstruction::Not(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Not(integer_type))
                }
                BeastInstruction::Neg(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Neg(integer_type))
                }
                BeastInstruction::Inc(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Inc(integer_type))
                }
                BeastInstruction::Dec(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Dec(integer_type))
                }
                BeastInstruction::U8Promote => MetaInstr::ActualInstr(MelonInstruction::U8Promote),
                BeastInstruction::U16Demote => MetaInstr::ActualInstr(MelonInstruction::U16Demote),
                BeastInstruction::I8Promote => MetaInstr::ActualInstr(MelonInstruction::I8Promote),
                BeastInstruction::I16Demote => MetaInstr::ActualInstr(MelonInstruction::I16Demote),
                BeastInstruction::PushConstU8(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&consts, id)?;

                        MetaInstr::ActualInstr(MelonInstruction::PushConstU8(value as u8))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(MelonInstruction::PushConstU8(lit))
                    }
                },
                BeastInstruction::PushConstU16(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&consts, id)?;

                        MetaInstr::ActualInstr(MelonInstruction::PushConstU16(value as u16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(MelonInstruction::PushConstU16(lit))
                    }
                },
                BeastInstruction::PushConstI8(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&consts, id)?;

                        MetaInstr::ActualInstr(MelonInstruction::PushConstI8(value as i8))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(MelonInstruction::PushConstI8(lit))
                    }
                },
                BeastInstruction::PushConstI16(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&consts, id)?;

                        MetaInstr::ActualInstr(MelonInstruction::PushConstI16(value as i16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(MelonInstruction::PushConstI16(lit))
                    }
                },
                BeastInstruction::LoadReg(reg) => {
                    MetaInstr::ActualInstr(MelonInstruction::LoadReg(reg))
                }

                BeastInstruction::Load(integer_type, arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&consts, id)?;

                        MetaInstr::ActualInstr(MelonInstruction::Load(integer_type, value as u16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(MelonInstruction::Load(integer_type, lit))
                    }
                },
                BeastInstruction::LoadIndirect(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::LoadIndirect(integer_type))
                }
                BeastInstruction::Store(integer_type, arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&consts, id)?;

                        MetaInstr::ActualInstr(MelonInstruction::Store(integer_type, value as u16))
                    }
                    Argument::Literal(lit) => {
                        MetaInstr::ActualInstr(MelonInstruction::Store(integer_type, lit))
                    }
                },
                BeastInstruction::StoreIndirect(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::StoreIndirect(integer_type))
                }

                BeastInstruction::Dup(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Dup(integer_type))
                }
                BeastInstruction::Drop(integer_type) => {
                    MetaInstr::ActualInstr(MelonInstruction::Drop(integer_type))
                }
                BeastInstruction::Sys(mut signal) => {
                    signal.remove(0);

                    let real_signal = if signal == "halt" {
                        0
                    } else {
                        let signals = self.config
                            .clone()
                            .signals
                            .ok_or(format_err!("no signals were given"))?;

                        *signals.get(&signal).ok_or(format_err!(
                            "undefined signal {:?}. Available signals are {:?}",
                            signal,
                            signals.keys().cloned().collect::<Vec<_>>()
                        ))?
                    };

                    MetaInstr::ActualInstr(MelonInstruction::SysCall(real_signal))
                }
                BeastInstruction::Call(func_name) => {
                    let opt_import = imports.iter().find(|import| import.alias == func_name);
                    if let Some(ref import) = opt_import {
                        MetaInstr::Call {
                            this_func: import.origin_name.clone(),
                            in_this_module: import.module_path.clone(),
                        }
                    } else {
                        let opt_local = other_funcs
                            .iter()
                            .find(|other_func| other_func.name == func_name);
                        if let Some(ref local) = opt_local {
                            let exported_func =
                                exports.iter().find(|exp| exp.origin_name == local.name);

                            let func_name = if let Some(exp) = exported_func {
                                exp.alias.clone()
                            } else {
                                format!("{}{}", PRIVATE_PREFIX, local.name)
                            };

                            MetaInstr::Call {
                                this_func: func_name,
                                in_this_module: local_module_path.clone(),
                            }
                        } else {
                            bail!(
                                "unable to find function definition or import for {:?}",
                                func_name
                            );
                        }
                    }
                }
                BeastInstruction::Ret => MetaInstr::ActualInstr(MelonInstruction::Ret),
                BeastInstruction::Alloc(arg) => match arg {
                    Argument::Constant(id) => {
                        let value = Compiler::find_const(&consts, id)?;

                        MetaInstr::ActualInstr(MelonInstruction::Alloc(value as u16))
                    }
                    Argument::Literal(lit) => MetaInstr::ActualInstr(MelonInstruction::Alloc(lit)),
                },
                BeastInstruction::Free => MetaInstr::ActualInstr(MelonInstruction::Free),
                _ => unreachable!(),
            };

            meta_vec.push(meta_instr);
        }

        Ok(meta_vec)
    }

    fn find_const(consts: &Vec<Const>, id: String) -> Result<i32> {
        let cons = consts
            .iter()
            .find(|con| con.name == id)
            .ok_or(format_err!("unable to find constant: {:?}", id))?;

        Ok(cons.value)
    }
}
