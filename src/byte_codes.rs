use crate::error::ParseError;
use crate::parser::{Expr, ReturnType, Stmt, AST, FLOAT, INT};
use crate::token::Position;

use crate::stdlib::{collections::HashMap, convert::TryFrom, mem};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum OpCodes {
    Noop = 0x00,

    LoadInt = 0x01,
    LoadString = 0x02,
    FnCall = 0x03,
    FnCallNative = 0x04,
    LoadVariable = 0x05,
    LoadVariableX1 = 0x06,
    LoadVariableX = 0x07,
    LoadArray = 0x08,
    LoadMap = 0x09,
    SetVariable = 0x0c,
    SetConstant = 0x0d,
    GetProperty = 0x0e,
    SetProperty = 0x0f,

    Jump = 0x30,
    BranchIfTrue = 0x31,
    BranchIfTrueCopy = 0x32,
    BranchIfFalse = 0x33,
    BranchIfFalseCopy = 0x34,

    #[cfg(not(feature = "no_float"))]
    LoadFloat = 0x80,

    LoadChar = 0x81,
    LoadTrue = 0x82,
    LoadFalse = 0x83,
    LoadUnit = 0x84,

    Negate = 0x90,
    In = 0x91,
    Continue = 0x92,
    Break = 0x93,
    Return = 0x94,
    Throw = 0x95,

    PopStack = 0xf0,
    PushFrame = 0xf1,
    PopFrame = 0xf2,
    PushLoop = 0xf3,
    PopLoop = 0xf4,
}

fn get_op_code_offset(offset: usize) -> u8 {
    if offset <= u8::MAX as usize {
        0
    } else if offset <= u16::MAX as usize {
        0x10
    } else {
        0x20
    }
}

fn make_room(stream: &mut Vec<u8>, size: usize) -> usize {
    let start = stream.len();
    if size > 0 {
        (0..size).into_iter().for_each(|_| stream.push(0));
    }
    start
}

fn push_offset(stream: &mut Vec<u8>, offset: usize) -> usize {
    if offset <= u8::MAX as usize {
        stream.push(offset as u8);
        stream.len() - 1
    } else if offset <= u16::MAX as usize {
        let start = make_room(stream, 2);
        stream[start..][..2].copy_from_slice(&(offset as u16).to_le_bytes());
        start
    } else {
        let start = make_room(stream, mem::size_of::<usize>());
        stream[start..][..mem::size_of::<usize>()].copy_from_slice(&offset.to_le_bytes());
        start
    }
}

fn push_string(text: &str, dict: &mut HashMap<String, usize>, constants: &mut Vec<u8>) -> usize {
    if let Some(offset) = dict.get(text) {
        *offset
    } else {
        let size = text.as_bytes().len();

        if size < u8::MAX as usize {
            let offset = constants.len();
            constants.push(size as u8);
            if size > 0 {
                make_room(constants, size);
                constants[offset + 1..][..size].copy_from_slice(text.as_bytes());
            }
            dict.insert(text.to_string(), offset);
            offset
        } else {
            let offset = make_room(constants, size + 1 + mem::size_of::<usize>());
            constants.push(u8::MAX);
            constants[offset + 1..][..mem::size_of::<usize>()].copy_from_slice(&size.to_le_bytes());
            constants[offset + 1 + mem::size_of::<usize>()..][..size]
                .copy_from_slice(text.as_bytes());
            dict.insert(text.to_string(), offset);
            offset
        }
    }
}

fn read_offset(stream: &[u8], pc: &mut usize, op_code_offset: u8) -> usize {
    match op_code_offset {
        0x00 => {
            let result = stream[*pc] as usize;
            *pc += 1;
            result
        }
        0x10 => {
            let mut data = [0_u8; mem::size_of::<u16>()];
            data.copy_from_slice(&stream[*pc..][..mem::size_of::<u16>()]);
            let result = u16::from_le_bytes(data) as usize;
            *pc += data.len();
            result
        }
        0x20 => {
            let mut data = [0_u8; mem::size_of::<usize>()];
            data.copy_from_slice(&stream[*pc..][..mem::size_of::<usize>()]);
            let result = usize::from_le_bytes(data) as usize;
            *pc += data.len();
            result
        }
        _ => unreachable!(),
    }
}

fn read_string(stream: &[u8], offset: usize) -> String {
    let (offset, len) = if stream[offset] < u8::MAX {
        (offset + 1, stream[offset] as usize)
    } else {
        let mut data = [0_u8; mem::size_of::<usize>()];
        data.copy_from_slice(&stream[offset + 1..][..mem::size_of::<usize>()]);
        (
            offset + 1 + mem::size_of::<usize>(),
            usize::from_le_bytes(data),
        )
    };

    String::from_utf8(stream[offset..][..len].to_vec()).unwrap()
}

fn print_program(op_codes: &[u8], constants: &[u8]) {
    let mut pc = 0;

    while pc < op_codes.len() {
        print!("{:<5}: ", pc);

        let op_code = op_codes[pc];
        pc += 1;

        let (op_code, op_code_offset) =
            if (0..0x10).contains(&op_code) || (0x30..0x40).contains(&op_code) {
                (op_code, 0)
            } else if (0x10..0x20).contains(&op_code) || (0x40..0x50).contains(&op_code) {
                (op_code - 0x10, 0x10)
            } else if (0x20..0x30).contains(&op_code) || (0x50..0x60).contains(&op_code) {
                (op_code - 0x20, 0x20)
            } else {
                (op_code, 0)
            };

        match op_code {
            0x00 => println!("Noop"),

            0x01 => {
                let value = read_offset(op_codes, &mut pc, op_code_offset) as INT;
                println!("LoadInt {}", value);
            }
            0x02 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                let value = read_string(constants, offset);
                println!(r#"LoadString "{}""#, value);
            }
            0x03 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                let value = read_string(constants, offset);
                let args = op_codes[pc];
                pc += 1;
                println!(r#"FnCall {}({})"#, value, args);
            }
            0x04 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                let value = read_string(constants, offset);
                let args = op_codes[pc];
                pc += 1;
                println!(r#"FnCallNative {}({})"#, value, args);
            }
            0x05 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                let name = read_string(constants, offset);
                println!(r#"LoadVariable {}"#, name);
            }
            0x06 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                let name = read_string(constants, offset);
                let index = op_codes[pc];
                pc += 1;
                println!(r#"LoadVariable {}:{}"#, name, index);
            }
            0x07 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                let name = read_string(constants, offset);
                let mut data = [0_u8; mem::size_of::<usize>()];
                data.copy_from_slice(&op_codes[pc..][..mem::size_of::<usize>()]);
                let index = usize::from_le_bytes(data);
                pc += mem::size_of::<usize>();
                println!(r#"LoadVariable {}:{}"#, name, index);
            }
            0x08 => {
                let items = read_offset(op_codes, &mut pc, op_code_offset);
                println!("LoadArray {}", items);
            }
            0x09 => println!("LoadMap"),
            0x0c => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                let name = read_string(constants, offset);
                println!(r#"SetVariable {}"#, name);
            }
            0x0d => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                let name = read_string(constants, offset);
                println!(r#"SetConstant {}"#, name);
            }
            0x0e => println!("GetProperty"),
            0x0f => println!("SetProperty"),

            0x30 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                println!("Jump {}", offset);
            }
            0x31 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                println!("BranchIfTrue {}", offset);
            }
            0x32 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                println!("BranchIfTrueCopy {}", offset);
            }
            0x33 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                println!("BranchIfFalse {}", offset);
            }
            0x34 => {
                let offset = read_offset(op_codes, &mut pc, op_code_offset);
                println!("BranchIfFalseCopy {}", offset);
            }

            #[cfg(not(feature = "no_float"))]
            0x80 => {
                let mut data = [0_u8; mem::size_of::<FLOAT>()];
                data.copy_from_slice(&op_codes[pc..][..mem::size_of::<FLOAT>()]);
                pc += mem::size_of::<FLOAT>();
                let value = FLOAT::from_le_bytes(data);
                println!("LoadFloat {:0.4}", value);
            }
            0x81 => {
                let mut data = [0_u8; mem::size_of::<u32>()];
                data.copy_from_slice(&op_codes[pc..][..mem::size_of::<u32>()]);
                pc += mem::size_of::<u32>();
                let ch = char::try_from(u32::from_le_bytes(data)).unwrap();
                println!("LoadChar {} ({:04x})", ch, ch as u32);
            }
            0x82 => println!("LoadTrue"),
            0x83 => println!("LoadFalse"),
            0x84 => println!("LoadUnit"),

            0x90 => println!("Negate"),
            0x91 => println!("In"),
            0x92 => println!("Continue"),
            0x93 => println!("Break"),
            0x94 => println!("Return"),
            0x95 => println!("Throw"),

            0xf0 => println!("PopStack"),
            0xf1 => println!("PushFrame"),
            0xf2 => println!("PopFrame"),
            0xf3 => println!("PushLoop"),
            0xf4 => println!("PopLoop"),

            _ => panic!("Unknown op-code {:x}", op_code),
        }
    }
}

fn compile_branch(
    branch_op: OpCodes,
    op_codes: &mut Vec<u8>,
    dict: &mut HashMap<String, usize>,
    constants: &mut Vec<u8>,
    mut func: impl FnMut(&mut Vec<u8>, &mut HashMap<String, usize>, &mut Vec<u8>) -> usize,
) -> usize {
    let orig_op_len = op_codes.len();
    let orig_constants_len = constants.len();

    let mut op_code_addr_offset = 0x00 as u8;

    loop {
        op_codes.push(branch_op as u8 + op_code_addr_offset);

        let bytes = match op_code_addr_offset {
            0x00 => mem::size_of::<u8>(),
            0x10 => mem::size_of::<u16>(),
            0x20 => mem::size_of::<usize>(),
            _ => unreachable!(),
        };

        (0..bytes).for_each(|_| op_codes.push(0));

        let pc = func(op_codes, dict, constants);

        match op_code_addr_offset {
            0x00 if pc <= u8::MAX as usize => {
                op_codes[orig_op_len + 1] = pc as u8;
                return orig_op_len + 1 + bytes;
            }
            0x10 if pc <= u16::MAX as usize => {
                op_codes[orig_op_len + 1..][..bytes].copy_from_slice(&(pc as u16).to_le_bytes());
                return orig_op_len + 1 + bytes;
            }
            0x20 => {
                op_codes[orig_op_len + 1..][..bytes].copy_from_slice(&pc.to_le_bytes());
                return orig_op_len + 1 + bytes;
            }
            _ if pc <= u16::MAX as usize => op_code_addr_offset = 0x10,
            _ => op_code_addr_offset = 0x20,
        }

        // Start again
        op_codes.truncate(orig_op_len);
        constants.truncate(orig_constants_len);
        dict.retain(|_, v| *v < orig_constants_len);
    }
}

fn compile_stmt(
    stmt: &Stmt,
    op_codes: &mut Vec<u8>,
    dict: &mut HashMap<String, usize>,
    constants: &mut Vec<u8>,
) -> bool {
    match stmt {
        Stmt::Noop(_) => false,

        Stmt::IfThenElse(x) => {
            let (expr, then_stmt, else_stmt) = x.as_ref();

            compile_expr(expr, op_codes, dict, constants);
            compile_branch(
                OpCodes::BranchIfFalse,
                op_codes,
                dict,
                constants,
                |op_codes, dict, constants| {
                    if !compile_stmt(then_stmt, op_codes, dict, constants) {
                        op_codes.push(OpCodes::LoadUnit as u8);
                    }

                    compile_branch(
                        OpCodes::Jump,
                        op_codes,
                        dict,
                        constants,
                        |op_codes, dict, constants| {
                            if let Some(stmt) = else_stmt {
                                if !compile_stmt(stmt, op_codes, dict, constants) {
                                    op_codes.push(OpCodes::LoadUnit as u8);
                                }
                            } else {
                                op_codes.push(OpCodes::LoadUnit as u8);
                            }
                            op_codes.len()
                        },
                    )
                },
            );
            true
        }

        Stmt::While(x) => {
            let (expr, stmt) = x.as_ref();

            op_codes.push(OpCodes::PushLoop as u8);
            let pc = op_codes.len();
            compile_expr(expr, op_codes, dict, constants);
            compile_branch(
                OpCodes::BranchIfFalse,
                op_codes,
                dict,
                constants,
                |op_codes, dict, constants| {
                    if compile_stmt(stmt, op_codes, dict, constants) {
                        op_codes.push(OpCodes::PopStack as u8);
                    }
                    op_codes.push(OpCodes::Jump as u8);
                    push_offset(op_codes, pc);
                    op_codes.len()
                },
            );
            op_codes.push(OpCodes::PopLoop as u8);
            false
        }

        Stmt::Loop(x) => {
            op_codes.push(OpCodes::PushLoop as u8);
            let pc = op_codes.len();
            compile_stmt(x.as_ref(), op_codes, dict, constants);
            op_codes.push(OpCodes::Jump as u8 + get_op_code_offset(pc));
            push_offset(op_codes, pc);
            //op_codes.push(OpCodes::PopLoop as u8);    // There is no way to exit this loop
            false
        }

        Stmt::For(_) => false,

        Stmt::Let(x) => {
            let ((name, _), expr) = x.as_ref();

            if let Some(expr) = expr {
                compile_expr(expr, op_codes, dict, constants);
            } else {
                op_codes.push(OpCodes::LoadUnit as u8);
            }

            let offset = push_string(name, dict, constants);
            op_codes.push(OpCodes::SetVariable as u8 + get_op_code_offset(offset));
            push_offset(op_codes, offset);
            false
        }
        Stmt::Const(x) => {
            let ((name, _), expr) = x.as_ref();

            compile_expr(expr, op_codes, dict, constants);

            let offset = push_string(name, dict, constants);
            op_codes.push(OpCodes::SetConstant as u8 + get_op_code_offset(offset));
            push_offset(op_codes, offset);
            false
        }

        Stmt::Block(x) => {
            let (statements, _) = x.as_ref();

            let len = statements.len() - 1;
            let mut has_return = false;

            op_codes.push(OpCodes::PushFrame as u8);

            for (i, stmt) in statements.iter().enumerate() {
                has_return = compile_stmt(stmt, op_codes, dict, constants);

                if has_return && i < len {
                    // Pop all statement values except the very last one
                    op_codes.push(OpCodes::PopStack as u8);
                }
            }

            op_codes.push(OpCodes::PopFrame as u8);

            has_return
        }
        Stmt::Expr(expr) => {
            compile_expr(expr.as_ref(), op_codes, dict, constants);
            true
        }
        Stmt::Continue(_) => {
            op_codes.push(OpCodes::Continue as u8);
            false
        }
        Stmt::Break(_) => {
            op_codes.push(OpCodes::Break as u8);
            false
        }
        Stmt::ReturnWithVal(x) if (x.0).0 == ReturnType::Return => {
            let (_, expr) = x.as_ref();

            if let Some(expr) = expr {
                compile_expr(expr, op_codes, dict, constants);
            } else {
                compile_expr(
                    &Expr::StringConstant(Box::new(("".into(), Position::none()))),
                    op_codes,
                    dict,
                    constants,
                );
            }

            op_codes.push(OpCodes::Return as u8);
            false
        }
        Stmt::ReturnWithVal(x) if (x.0).0 == ReturnType::Exception => {
            let (_, expr) = x.as_ref();

            if let Some(expr) = expr {
                compile_expr(expr, op_codes, dict, constants);
            } else {
                op_codes.push(OpCodes::LoadUnit as u8);
            }

            op_codes.push(OpCodes::Throw as u8);
            false
        }
        Stmt::ReturnWithVal(_) => unreachable!(),

        Stmt::Import(_) => false,
        Stmt::Export(_) => false,
    }
}

fn compile_expr(
    expr: &Expr,
    op_codes: &mut Vec<u8>,
    dict: &mut HashMap<String, usize>,
    constants: &mut Vec<u8>,
) {
    match expr {
        Expr::Expr(x) => compile_expr(x.as_ref(), op_codes, dict, constants),

        #[cfg(not(feature = "no_float"))]
        Expr::FloatConstant(f) => {
            let data = f.0.to_le_bytes();
            let size = data.len();
            op_codes.push(OpCodes::LoadFloat as u8);
            let offset = make_room(op_codes, size);
            op_codes[offset..][..size].copy_from_slice(&data);
        }

        Expr::IntegerConstant(i) => {
            let value = i.0 as usize;

            if value <= u8::MAX as usize {
                op_codes.push(OpCodes::LoadInt as u8);
                op_codes.push(value as u8);
            } else if value <= u16::MAX as usize {
                op_codes.push(OpCodes::LoadInt as u8 + 0x10);
                let start = make_room(op_codes, mem::size_of::<u16>());
                op_codes[start..][..mem::size_of::<u16>()]
                    .copy_from_slice(&(value as u16).to_le_bytes());
            } else {
                op_codes.push(OpCodes::LoadInt as u8 + 0x20);
                let start = make_room(op_codes, mem::size_of::<usize>());
                op_codes[start..][..mem::size_of::<usize>()].copy_from_slice(&value.to_le_bytes());
            }
        }
        Expr::CharConstant(c) => {
            op_codes.push(OpCodes::LoadChar as u8);
            let offset = make_room(op_codes, mem::size_of::<u32>());
            op_codes[offset..][..mem::size_of::<u32>()]
                .copy_from_slice(&(c.0 as u32).to_le_bytes());
        }
        Expr::True(_) => op_codes.push(OpCodes::LoadTrue as u8),
        Expr::False(_) => op_codes.push(OpCodes::LoadFalse as u8),
        Expr::Unit(_) => op_codes.push(OpCodes::LoadUnit as u8),
        Expr::StringConstant(s) => {
            let offset = push_string(s.0.as_ref(), dict, constants);
            op_codes.push(OpCodes::LoadString as u8 + get_op_code_offset(offset));
            push_offset(op_codes, offset);
        }

        Expr::Variable(x) => {
            let ((name, _), _, _, index) = x.as_ref();

            let offset = push_string(name, dict, constants);

            if let Some(index) = index {
                let index = index.get();
                if index <= u8::MAX as usize {
                    op_codes.push(OpCodes::LoadVariableX1 as u8 + get_op_code_offset(offset));
                    push_offset(op_codes, offset);
                    op_codes.push(index as u8);
                } else {
                    op_codes.push(OpCodes::LoadVariableX as u8 + get_op_code_offset(offset));
                    push_offset(op_codes, offset);
                    let index_addr = make_room(op_codes, mem::size_of::<usize>());
                    op_codes[index_addr..][..mem::size_of::<usize>()]
                        .copy_from_slice(&index.to_le_bytes());
                }
            } else {
                op_codes.push(OpCodes::LoadVariable as u8 + get_op_code_offset(offset));
                push_offset(op_codes, offset);
            }
        }

        Expr::Stmt(x) => {
            let mut need_pop = false;
            if matches!(x.0, Stmt::Block(_)) {
                op_codes.push(OpCodes::PushFrame as u8);
                need_pop = true;
            }

            if !compile_stmt(&x.0, op_codes, dict, constants) {
                op_codes.push(OpCodes::LoadUnit as u8);
            }

            if need_pop {
                op_codes.push(OpCodes::PopFrame as u8);
            }
        }

        Expr::Property(_) | Expr::Dot(_) | Expr::Index(_) | Expr::Map(_) => unimplemented!(),

        Expr::Array(x) => {
            let (items, _) = x.as_ref();

            items
                .iter()
                .for_each(|expr| compile_expr(expr, op_codes, dict, constants));

            op_codes.push(OpCodes::LoadArray as u8);
            push_offset(op_codes, items.len());
        }

        Expr::Assignment(_) => (),
        Expr::FnCall(x) => {
            let ((name, native, _), _, _, args, _) = x.as_ref();

            args.iter()
                .for_each(|expr| compile_expr(expr, op_codes, dict, constants));

            let offset = push_string(name, dict, constants);

            op_codes.push(
                if *native {
                    OpCodes::FnCallNative
                } else {
                    OpCodes::FnCall
                } as u8
                    + get_op_code_offset(offset),
            );
            push_offset(op_codes, offset);
            op_codes.push(args.len() as u8); // TODO - check
        }

        Expr::In(x) => {
            let (lhs, rhs, _) = x.as_ref();
            compile_expr(lhs, op_codes, dict, constants);
            compile_expr(rhs, op_codes, dict, constants);
            op_codes.push(OpCodes::In as u8);
        }
        Expr::And(x) | Expr::Or(x) => {
            let (lhs, rhs, _) = x.as_ref();

            compile_expr(lhs, op_codes, dict, constants);

            let op = if matches!(expr, Expr::Or(_)) {
                OpCodes::BranchIfTrueCopy
            } else {
                OpCodes::BranchIfFalseCopy
            };

            compile_branch(
                op,
                op_codes,
                dict,
                constants,
                |op_codes, dict, constants| {
                    compile_expr(rhs, op_codes, dict, constants);
                    op_codes.len()
                },
            );
        }
    }
}

fn compile_ast(
    ast: &AST,
    op_codes: &mut Vec<u8>,
    dict: &mut HashMap<String, usize>,
    constants: &mut Vec<u8>,
) {
    let len = ast.statements().len() - 1;
    let mut has_return = false;

    for (i, stmt) in ast.statements().iter().enumerate() {
        has_return = compile_stmt(stmt, op_codes, dict, constants);

        // Pop all statement values except the very last one
        if has_return && i < len {
            op_codes.push(OpCodes::PopStack as u8);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::Stmt;
    use crate::*;

    #[test]
    fn test_compile_expr() -> Result<(), Box<EvalAltResult>> {
        let mut engine = Engine::new();
        engine.set_optimization_level(OptimizationLevel::None);
        let ast = engine.compile(
            r#"
                let now = timestamp();
                if now { let test = [1.0, 0x88, true, 'k', ()] }
                let x = 1_000_000;
                
                print("Ready... Go!");
                
                while x <= 0 {
                    x -= 1;
                }
                
                //print("Finished. Run time = " + now.elapsed + " seconds.");
                    "#,
        )?;

        let mut op_codes = Vec::new();
        let mut constants = Vec::new();
        let mut strings_dict = HashMap::new();

        compile_ast(&ast, &mut op_codes, &mut strings_dict, &mut constants);

        println!("{:x?}", constants);
        println!("{:x?}", op_codes);
        print_program(&op_codes, &constants);
        Ok(())
    }
}
