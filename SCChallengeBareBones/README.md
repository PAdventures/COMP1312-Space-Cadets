# SCChallengeBareBones

To run the project, you must have [Rust installed](https://rust-lang.org/tools/install/) and [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html).

To run the project, either:

1. Run `cargo run -- test.bb`
2. Run `cargo build -r` then run the binary `./target/release/scchallenge_bare_bones test.bb`

You must be in the root directory of the project to run the project or binary.

If you want to see more information about the interpreter, add the debug flag `-d` to the command.
This will pretty print the AST (Abstract Syntax Tree) and show you how long (in microseconds 😎) each step in the interpreter took.

The interpreter will always print the contents of each variable to the console.
