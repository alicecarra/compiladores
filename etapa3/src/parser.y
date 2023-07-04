%start program

%%

program-> Result<ASTNode, anyhow::Error>:
        item_list { $1 } |
        { Ok(ASTNode::None) } ;

item_list -> Result<ASTNode, anyhow::Error>:
        function item_list  {
                match $1? {
                        ASTNode::FunctionDeclaration(mut node) => {
                                let next_fn = Box::new($2?);
                                node.add_next_fn(next_fn);
                                Ok(ASTNode::FunctionDeclaration(node))
                        }
                        _ => bail!("Segundo elemento da produção incorreto."),
                }
        }|
        global_variable item_list { $2 } |
        function  { $1 }|
        global_variable { $1 };

global_variable -> Result<ASTNode, anyhow::Error>:
        type name_list ';' {  Ok(ASTNode::None) };

name_list -> Result<ASTNode, anyhow::Error>:
        identifier ',' name_list  { Ok(ASTNode::None) }|
        identifier { Ok(ASTNode::None) };

function -> Result<ASTNode, anyhow::Error>:
        identifier parameters "TK_OC_MAP" type function_body  {
                let ident = $1?;
                let command = Box::new($5?);
                let node = FunctionDeclaration::new($span, command, ident.span()?);
                Ok(ASTNode::FunctionDeclaration(node))
        } ;

parameters-> Result<ASTNode, anyhow::Error>:
        '(' parameters_list ')' { Ok(ASTNode::None) } |
        '(' ')' { Ok(ASTNode::None) };

parameters_list-> Result<ASTNode, anyhow::Error>:
        type identifier ',' parameters_list { Ok(ASTNode::None) }| 
        type identifier { Ok(ASTNode::None) };

function_body-> Result<ASTNode, anyhow::Error>:
        '{' command_block '}' { $2 } |
        '{' '}' { Ok(ASTNode::None) };

command_block-> Result<ASTNode, anyhow::Error>:
        command command_block{
                let command = $1?;
                match command {
                        ASTNode::None => $2,
                        _ => {
                                let next = Box::new($2?);
                                let node = command.add_next(next)?;
                                Ok(node)
                        },
                }
        }|
        command { $1 };

command-> Result<ASTNode, anyhow::Error>:
        variable ';' { $1 } | 
        assignment ';' { $1 } | 
        function_call ';'  { $1 }| 
        return ';'  { $1 }| 
        flow_ctrl ';'  { $1 }| 
        function_body ';'  { $1 };

variable ->     Result<ASTNode, anyhow::Error>:
        type identifier ',' name_with_value_list { Ok(ASTNode::None) } |
        type identifier "TK_OC_LE" literal ',' name_with_value_list {
                let ident = Box::new($2?);
                let lit = Box::new($4?);
                let next = Some(Box::new($6?));
                let node = InitializedVariable::new($span, ident, lit, next);
                Ok(ASTNode::InitializedVariable(node))
        } |
        type identifier { Ok(ASTNode::None) } |
        type identifier "TK_OC_LE" literal {
                let ident = Box::new($2?);
                let lit = Box::new($4?);
                let node = InitializedVariable::new($span, ident, lit, None);
                Ok(ASTNode::InitializedVariable(node))
        } ;

name_with_value_list-> Result<ASTNode, anyhow::Error>:
        identifier  "TK_OC_LE" literal ',' name_with_value_list {
                let ident = Box::new($1?);
                let lit = Box::new($3?);
                let next = Some(Box::new($5?));
                let node = InitializedVariable::new($span, ident, lit, next);
                Ok(ASTNode::InitializedVariable(node))
        }|
        identifier  "TK_OC_LE" literal {
                let ident = Box::new($1?);
                let lit = Box::new($3?);
                let node = InitializedVariable::new($span, ident, lit, None);
                Ok(ASTNode::InitializedVariable(node))
        } | 
        identifier ',' name_with_value_list { $3 }  | 
        identifier { Ok(ASTNode::None) };

assignment-> Result<ASTNode, anyhow::Error>:
        identifier '=' expression { 
                let ident = Box::new($1?);
                let expression = Box::new($3?);
                let node = AssignmentCommand::new($span, ident, expression);
                Ok(ASTNode::AssignmentCommand(node))
        };

function_call -> Result<ASTNode, anyhow::Error>:
        identifier arguments {
                let expression = Box::new($2?);
                let ident = $1?;
                let node = FunctionCallCommand::new($span, expression, ident.span()?);
                Ok(ASTNode::FunctionCallCommand(node))
        };
        
arguments-> Result<ASTNode, anyhow::Error>:
        '(' arguments_list ')'{ $2 }
        | '(' ')' { Ok(ASTNode::None) };

arguments_list-> Result<ASTNode, anyhow::Error>:
        expression ',' arguments_list {
                let expression = $1?;
                let next = Box::new($3?);
                let node = expression.add_next(next)?;
                Ok(node)
        } |
        expression  { $1 } ;

return ->       Result<ASTNode, anyhow::Error>:
        "TK_PR_RETURN" expression  {
                let expr = Box::new($2?);
                let node = ReturnCommand::new($span, expr);
                Ok(ASTNode::ReturnCommand(node))
        } ;

flow_ctrl ->    Result<ASTNode, anyhow::Error>:
        if       { $1 } |
        while    { $1 } ;

if->    Result<ASTNode, anyhow::Error>:
        "TK_PR_IF" '(' expression ')' function_body {
            let expr = Box::new($3?);
            let true_command = Box::new($5?);
            let node = IfCommand::new($span, expr, true_command, Box::new(ASTNode::None));
            Ok(ASTNode::IfCommand(node))
         }|  
        "TK_PR_IF" '(' expression ')' function_body "TK_PR_ELSE" function_body {
            let expr = Box::new($3?);
            let true_command = Box::new($5?);
            let false_command = Box::new($7?);
            let node = IfCommand::new($span, expr, true_command, false_command);
            Ok(ASTNode::IfCommand(node))
        };
while-> Result<ASTNode, anyhow::Error>:
        "TK_PR_WHILE"  '(' expression ')' function_body{
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
        "TK_LIT_TRUE"     { Ok(ASTNode::LiteralBool(LiteralBool::new($span))) } |
        "TK_LIT_FALSE"    { Ok(ASTNode::LiteralBool(LiteralBool::new($span))) } ;

type-> Result<ASTNode, anyhow::Error>:
        "TK_PR_INT" { Ok(ASTNode::None) } |
        "TK_PR_FLOAT" { Ok(ASTNode::None) } |
        "TK_PR_BOOL" { Ok(ASTNode::None) } ;

%%

use etapa3::ast::{
        InitializedVariable,
        AssignmentCommand,
        FunctionCallCommand,
        ReturnCommand,
        ASTNode,   
        BinaryOperation,
        UnaryOperation,
        IfCommand,
        WhileCommand,
        FunctionDeclaration,   
        LiteralInt,
        LiteralFloat,      
        LiteralBool,
        Identifier};
use anyhow::bail;
