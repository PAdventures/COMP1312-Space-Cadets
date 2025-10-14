// Import necessary modules
mod command;
mod frontend;
mod runtime;
use crate::{
    frontend::{ast, parser},
    runtime::{environment, interpreter::evaluate, values},
};
use frontend::lexer;
use std::{path::Path, process::exit, time::Instant};

// Define constants for exit codes
const EXIT_CODE_SUCCESS: i32 = 0;
const EXIT_CODE_SCANNER_ERROR: i32 = 65;
const EXIT_CODE_PARSER_ERROR: i32 = 65;
const EXIT_CODE_RUNTIME_ERROR: i32 = 70;

// Main entry point of the program
fn main() {
    // Get the current time, this is for debugging purposes
    let program_instance = Instant::now();

    // Get the arguments passed from the command line
    let arg_matches = command::command();

    // Get the string representation of the file path, this must be present
    if let Some(file) = arg_matches.get_one::<String>("file") {
        // Read the file contents into a string
        let source = match read_file(Path::new(file)) {
            Ok(source_code) => source_code,
            Err(err) => panic!("Failed to read file: {}", err),
        };

        // Create a new scanner instance with the file contents
        let mut scanner = lexer::Scanner::new(&source, true);

        // Time the scanner that tokenises the source code
        let scanner_instant = Instant::now();
        scanner.scan_tokens();
        let scanner_duration = scanner_instant.elapsed();

        // If there are errors, print them and exit
        if !scanner.errors.is_empty() {
            scanner.errors.iter().for_each(|error| {
                println!("{}", error.to_owned().to_string());
            });
            exit(EXIT_CODE_SCANNER_ERROR);
        }

        // Create a new parser instance with the scanner's token stream
        let mut parser = parser::Parser::new(&scanner.stream);

        // Time the parser that builds an Abstract Syntax Tree (AST)
        let parser_instant = Instant::now();
        let result = parser.parse_program();
        let parser_duration = parser_instant.elapsed();

        // If there are errors, print them and exit
        if !result.1.is_empty() {
            result
                .1
                .iter()
                .for_each(|error| println!("{}", error.to_string()));
            exit(EXIT_CODE_PARSER_ERROR);
        }

        // If the debug flag is set, print the AST in a human-readable format
        if arg_matches.get_flag("debug") {
            let ast_printer = ast::ASTPrinter::new();
            println!("{}", ast_printer.output(&result.0))
        }

        // Create a new variable environment for the program
        let mut env = environment::Environment::create();

        // Time the evaluator that reads the AST and executes the program
        let evaluation_instant = Instant::now();
        match evaluate(result.0, &mut env) {
            Ok(_) => {}

            // If the evaluator returns an error, print the error and exit with an error code
            Err(err) => {
                println!("{}", err);
                exit(EXIT_CODE_RUNTIME_ERROR);
            }
        }
        let evaluation_duration = evaluation_instant.elapsed();

        // Dump the contents of the variable environment
        for (key, value) in env.variables {
            println!(
                "{} = {}",
                key,
                match value {
                    values::RuntimeValue::Number(n) => n,
                    _ => panic!("Unexpected value type"),
                }
            )
        }

        // If the debug flag is set, print the duration of each phase of the interpreter
        //
        // They are all measured in microseconds because my rust code is just so efficient 😎
        if arg_matches.get_flag("debug") {
            println!("Scanning took {}us", scanner_duration.as_micros());
            println!("Parsing took {}us", parser_duration.as_micros());
            println!("Evaluation took {}us", evaluation_duration.as_micros());
            println!(
                "Total time taken {}us",
                program_instance.elapsed().as_micros()
            );
        }

        exit(EXIT_CODE_SUCCESS);
    }

    panic!("Required arg missing")
}

// Read a file from the filesystem and return its contents as a string
pub fn read_file(path: &Path) -> Result<String, String> {
    match std::fs::read_to_string(path) {
        Ok(source_code) => Ok(source_code),
        Err(err) => Err(err.to_string()),
    }
}

// Tests the interpreter to ensure any changes made to the code do not break existing functionality
#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{
        frontend::{lexer, parser},
        read_file,
        runtime::{environment, interpreter::evaluate, values},
    };

    // In this test, "X" is multiplied by "Y" and the result is assigned to "Z"
    //
    // "X" initially has a value of 2
    // "Y" initially has a value of 3
    //
    // At the end of the test, "Z" should have a value of 6
    #[test]
    fn example() {
        let source = match read_file(Path::new("test.bb")) {
            Ok(source_code) => source_code,
            Err(err) => panic!("Failed to read file: {}", err),
        };

        let mut scanner = lexer::Scanner::new(&source, true);
        scanner.scan_tokens();

        assert_eq!(scanner.errors.len(), 0);

        let mut parser = parser::Parser::new(&scanner.stream);

        let result = parser.parse_program();

        assert_eq!(result.1.len(), 0);

        let mut env = environment::Environment::create();

        match evaluate(result.0, &mut env) {
            Ok(_) => {}
            Err(err) => panic!("Runtime error: {}", err),
        }

        assert_eq!(env.variables.len(), 4);
        assert_eq!(
            env.lookup_variable("X".to_string()),
            Some(values::RuntimeValue::Number(0))
        );
        assert_eq!(
            env.lookup_variable("Y".to_string()),
            Some(values::RuntimeValue::Number(3))
        );
        assert_eq!(
            env.lookup_variable("W".to_string()),
            Some(values::RuntimeValue::Number(0))
        );
        assert_eq!(
            env.lookup_variable("Z".to_string()),
            Some(values::RuntimeValue::Number(6))
        );
    }
}
