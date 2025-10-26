// Import necessary modules
use crate::frontend::lexer::Token;

// Define the main definition of the AST
#[derive(Debug, Clone)]
pub enum ASTStatement {
    // The root of the AST, representing the entire program.
    Program(Vec<ASTStatement>),

    While(WhileLoopStatement),
    If(IfStatement),
    For(ForLoopStatement),
    // ForIn(ForInLoopStatement),
    Function(FunctionStatement),
    Expression(ASTExpression),
}

// A statement representing a function definition.
#[derive(Debug, Clone)]
pub struct FunctionStatement {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub body: Vec<ASTStatement>,
}

impl FunctionStatement {
    pub fn new(name: Token, parameters: Vec<Token>, body: Vec<ASTStatement>) -> Self {
        FunctionStatement {
            name,
            parameters,
            body,
        }
    }
}

// A statement representing an if statement.
#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: ASTExpression,
    pub body: Vec<ASTStatement>,
    pub branches: Vec<ElseStatement>,
}

impl IfStatement {
    pub fn new(
        condition: ASTExpression,
        body: Vec<ASTStatement>,
        branches: Vec<ElseStatement>,
    ) -> Self {
        IfStatement {
            condition,
            body,
            branches,
        }
    }
}

// A statement representing an else (or elif) statement.
#[derive(Debug, Clone)]
pub struct ElseStatement {
    pub condition: Option<ASTExpression>,
    pub body: Vec<ASTStatement>,
}

impl ElseStatement {
    pub fn new(condition: Option<ASTExpression>, body: Vec<ASTStatement>) -> Self {
        ElseStatement { condition, body }
    }
}

// A statement representing a for loop
#[derive(Debug, Clone)]
pub struct ForLoopStatement {
    pub condition: ASTExpression,
    pub step: VariableAssignmentExpression,
    pub body: Vec<ASTStatement>,
}

impl ForLoopStatement {
    pub fn new(
        condition: ASTExpression,
        step: VariableAssignmentExpression,
        body: Vec<ASTStatement>,
    ) -> Self {
        ForLoopStatement {
            condition,
            step,
            body,
        }
    }
}

// // A statement representing a for-in loop
// #[derive(Debug, Clone)]
// pub struct ForInLoopStatement {
//     pub variable: Token,
//     pub collection: ASTExpression,
//     pub body: Vec<ASTStatement>,
// }

// impl ForInLoopStatement {
//     pub fn new(variable: Token, collection: ASTExpression, body: Vec<ASTStatement>) -> Self {
//         ForInLoopStatement {
//             variable,
//             collection,
//             body,
//         }
//     }
// }

// A statement representing a while loop.
#[derive(Debug, Clone)]
pub struct WhileLoopStatement {
    pub condition: ASTExpression,
    pub body: Vec<ASTStatement>,
}

impl WhileLoopStatement {
    pub fn new(condition: ASTExpression, body: Vec<ASTStatement>) -> Self {
        WhileLoopStatement { condition, body }
    }
}

// A statement representing an expression.
#[derive(Debug, Clone)]
pub enum ASTExpression {
    Literal(ASTLiteral),
    // An expression representing a variable / identifier
    Variable(Token),
    VariableAssignment(VariableAssignmentExpression),
    Comparison(ComparisonExpression),
    Call(FunctionCallExpression),
    Return(Option<Box<ASTExpression>>),
    Logical(LogicalExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Grouping(Box<ASTExpression>),
}

// An expression representing a unary operation
#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: Token,
    pub right: Box<ASTExpression>,
}

impl UnaryExpression {
    pub fn new(operator: Token, right: Box<ASTExpression>) -> Self {
        UnaryExpression { operator, right }
    }
}

// An expression representing a binary operation.
#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub left: Box<ASTExpression>,
    pub operator: Token,
    pub right: Box<ASTExpression>,
    pub into: Token,
}

impl BinaryExpression {
    pub fn new(left: ASTExpression, operator: Token, right: ASTExpression, into: Token) -> Self {
        BinaryExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            into,
        }
    }
}

// An expression representing a logical operation.
#[derive(Debug, Clone)]
pub struct LogicalExpression {
    pub left: Box<ASTExpression>,
    pub operator: Token,
    pub right: Box<ASTExpression>,
}

impl LogicalExpression {
    pub fn new(left: ASTExpression, operator: Token, right: ASTExpression) -> Self {
        LogicalExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

// An expression representing a function call.
#[derive(Debug, Clone)]
pub struct FunctionCallExpression {
    pub function: Token,
    pub arguments: Vec<ASTExpression>,
}

impl FunctionCallExpression {
    pub fn new(function: Token, arguments: Vec<ASTExpression>) -> Self {
        FunctionCallExpression {
            function,
            arguments,
        }
    }
}

// An expression representing a variable assignment.
#[derive(Debug, Clone)]
pub struct VariableAssignmentExpression {
    pub variable: Token,
    pub command: Token,
    pub value: Option<Box<ASTExpression>>,
}

impl VariableAssignmentExpression {
    pub fn new(variable: Token, command: Token, value: Option<Box<ASTExpression>>) -> Self {
        VariableAssignmentExpression {
            variable,
            command,
            value,
        }
    }
}

// An expression representing a comparison.
#[derive(Debug, Clone)]
pub struct ComparisonExpression {
    pub left: Box<ASTExpression>,
    pub operator: Token,
    pub right: Box<ASTExpression>,
}

impl ComparisonExpression {
    pub fn new(left: ASTExpression, operator: Token, right: ASTExpression) -> Self {
        ComparisonExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

// An expression representing a literal value.
#[derive(Debug, Clone)]
pub enum ASTLiteral {
    Integer(i64),
    Float(f64),
    Character(char),
    String(String),
    Boolean(bool),
    // Array(Vec<ASTLiteral>),
    Null,
}
