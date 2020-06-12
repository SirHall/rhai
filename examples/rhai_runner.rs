use rhai::{Engine, EvalAltResult, Position};

#[cfg(not(feature = "no_optimize"))]
use rhai::OptimizationLevel;

use std::{env, fs::File, io::Read, process::exit};

fn eprint_error(input: &str, err: EvalAltResult) {
    fn eprint_line(lines: &[&str], pos: Position, err: &str) {
        let line = pos.line().unwrap();

        let line_no = format!("{}: ", line);
        let pos_text = format!(" ({})", pos);

        eprintln!("{}{}", line_no, lines[line - 1]);
        eprintln!(
            "{:>1$} {2}",
            "^",
            line_no.len() + pos.position().unwrap(),
            err.replace(&pos_text, "")
        );
        eprintln!("");
    }

    let lines: Vec<_> = input.split('\n').collect();

    // Print error
    let pos = err.position();

    if pos.is_none() {
        // No position
        eprintln!("{}", err);
    } else {
        // Specific position
        let err_text = match err {
            EvalAltResult::ErrorRuntime(err, _) if !err.is_empty() => {
                format!("Runtime error: {}", err)
            }
            err => err.to_string(),
        };

        eprint_line(&lines, pos, &err_text)
    }
}

fn main() {
    for filename in env::args().skip(1) {
        let mut engine = Engine::new();

        #[cfg(not(feature = "no_optimize"))]
        engine.set_optimization_level(OptimizationLevel::Full);

        let mut f = match File::open(&filename) {
            Err(err) => {
                eprintln!("Error reading script file: {}\n{}", filename, err);
                exit(1);
            }
            Ok(f) => f,
        };

        let mut contents = String::new();

        if let Err(err) = f.read_to_string(&mut contents) {
            eprintln!("Error reading script file: {}\n{}", filename, err);
            exit(1);
        }

        if let Err(err) = engine.consume(&contents) {
            eprintln!("{:=<1$}", "", filename.len());
            eprintln!("{}", filename);
            eprintln!("{:=<1$}", "", filename.len());
            eprintln!("");

            eprint_error(&contents, *err);
        }
    }
}
