%start program

%%

program->;
item_list ->;

global_variable ->; 
name_list ->;

function ->;
function_header->;

parameters->;
parameters_list->;

function_body->;
command_block->;
command->;

variable ->; 
name_with_value_list->;

assignment->;

function_call ->;
arguments->;
arguments_list->;

return ->;

flow_ctrl ->;

if->    Result<ASTNode, anyhow::Error>:
        TK_PR_IF '(' expression ')' function_body {
            let expr = Box::new($3?);
            let true_command = Box::new($6?);
            let node = IfCommand::new($span, expr, true_command, Box::new(ASTNode::None));
            Ok(ASTNode::IfCommand(node))
         }|  
        TK_PR_IF '(' expression ')' function_body TK_PR_ELSE function_body {
            let expr = Box::new($3?);
            let true_command = Box::new($6?);
            let false_command = Box::new($8?);
            let node = IfCommand::new($span, expr, true_command, false_command);
            Ok(ASTNode::IfCommand(node))
        };
while-> Result<ASTNode, anyhow::Error>:
        TK_PR_WHILE  '(' expression ')' function_body{
            let expr = Box::new($3?);
            let command = Box::new($5?);
            let node = WhileCommand::new($span, expr, command);
            Ok(ASTNode::WhileCommand(node))
        }; 

expression -> Result<ASTNode, anyhow::Error>:
        expression "TK_OC_OR" expression2 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::OrExpression(node))
        } |
        expression2  { $1 } ;
expression2-> Result<ASTNode, anyhow::Error>:
        expression2 "TK_OC_AND" expression3 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::AndExpression(node))
        } |
        expression3  { $1 } ;
expression3->  Result<ASTNode, anyhow::Error>:
        expression3 "TK_OC_EQ" expression4 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::EqualExpression(node))
        } |
        expression3 "TK_OC_NE" expression4 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::NotEqualExpression(node))
        } |
        expression4  { $1 } ;
expression4-> Result<ASTNode, anyhow::Error>:
        expression4 '<' expression5 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::LessThanExpression(node))
         } |
        expression4 '>' expression5 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::GreaterThanExpression(node))
         } |
        expression4 "TK_OC_LE" expression5 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::LessEqualExpression(node))
         } |
        expression4 "TK_OC_GE" expression5 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::GreaterEqualExpression(node))
         } |
        expression5  { $1 } ;
expression5->  Result<ASTNode, anyhow::Error>:
        expression5 '+' expression6 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::AdditionExpression(node))
         } |
        expression5 '-' expression6 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::SubtractionExpression(node))
         } |
        expression6  { $1 } ;
expression6-> Result<ASTNode, anyhow::Error>:
        expression6 '*' expression7 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::MultiplyExpression(node))
        } |
        expression6 '/' expression7 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::DivisionExpression(node))
        } |
        expression6 '%' expression7 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right);
                Ok(ASTNode::ModExpression(node))
        } |
        expression7  { $1 } ;
expression7-> Result<ASTNode, anyhow::Error>:
        '-' expression8 {
                let child = Box::new($2?);
                let node = UnaryOperation::new($span, child);
                Ok(ASTNode::MinusExpression(node))
        } |
        '!' expression8 {
                let child = Box::new($2?);
                let node = UnaryOperation::new($span, child);
                Ok(ASTNode::NegateExpression(node))
        } |
        expression8  { $1 } ;
expression8-> Result<ASTNode, anyhow::Error>:
        '(' expression ')'    { $2 } |
        operand         { $1 } ;

operand -> Result<ASTNode, anyhow::Error>:
        identifier      { $1 } |        
        literal         { $1 } ;

identifier -> Result<ASTNode, anyhow::Error>:
        "TK_IDENTIFICADOR" { Ok(ASTNode::Identifier(Identifier::new($span)))} ;

literal -> Result<ASTNode, anyhow::Error>:
        "TK_LIT_INT"      { Ok(ASTNode::LiteralInt(LiteralInt::new($span))) } |
        "TK_LIT_FLOAT"    { Ok(ASTNode::LiteralFloat(LiteralFloat::new($span))) } |
        "TK_LIT_CHAR"     { Ok(ASTNode::LiteralChar(LiteralChar::new($span))) } |
        "TK_LIT_TRUE"     { Ok(ASTNode::LiteralBool(LiteralBool::new($span))) } |
        "TK_LIT_FALSE"    { Ok(ASTNode::LiteralBool(LiteralBool::new($span))) } ;

type-> Result<ASTNode, anyhow::Error>:
        "TK_PR_INT" { Ok(ASTNode::None) } |
        "TK_PR_FLOAT" { Ok(ASTNode::None) } |
        "TK_PR_BOOL" { Ok(ASTNode::None) } |
        "TK_PR_CHAR"  { Ok(ASTNode::None) } ;

%%

use etapa3::ast::{
        ASTNode,   
        BinaryOperation,
        UnaryOperation,
        LitInt,
        LitFloat,
        LitChar,
        LitBool,
        Identifier};
use anyhow::bail;
