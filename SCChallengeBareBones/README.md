# SCChallengeBareBones

To run the project, you must have [Rust installed](https://rust-lang.org/tools/install/) and [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html).

To run the project, either:

1. Run `cargo run -- test.bb`
2. Run `cargo build -r` then run the binary `./target/release/scchallenge_bare_bones test.bb`

You must be in the root directory of the project to run the project or binary.

If you want to see more information about the interpreter, add the debug flag `-d` to the command.
This will pretty print the AST (Abstract Syntax Tree) and show you how long (in microseconds 😎) each step in the interpreter took.

The interpreter will always print the contents of each variable to the console.

## Extended Bare Bones (Not so Bare Bones anymore)

The extended bare bones interpreter is an extension of the base interpreter that adds support for more features.
Below are the features that have been added:

- Comments (`//`)
- Expression grouping (`()`)
- Binary arithmetic (add, sub, mul, div)
- More variable assignment (set, copy):
  - Increment can now optionally specify a step value (`incr X by 2`)
  - Decrement can now optionally specify a step value (`decr X by 2`)
- If statements (if, elif, else)
- For loops (for)
- Functions (fn, param, ret, call, arg)
- More data types:
  - Integers (allow negatives)
  - Booleans
  - Floats
  - Chars
  - Strings
- Comparisons:
  - Equal to (`eq`) replaces `is`
  - Not equal to (`ne`) replaces `not`
  - Less than (`lt`)
  - Greater than (`gt`)
  - Less than or equal to (`le`)
  - Greater than or equal to (`ge`)
- Logical operators (and, or)
- Unary operators (neg, not)
