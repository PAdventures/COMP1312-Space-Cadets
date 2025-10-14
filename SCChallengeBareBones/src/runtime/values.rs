// All the values that can be returned during runtime
#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeValue {
    Number(u64), // Only this is exposed as a public value
    Boolean(bool),
    Null,
}
