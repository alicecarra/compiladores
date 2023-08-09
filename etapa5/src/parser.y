%start program

%%

program-> Result<ASTNode, ParsingError>:
        item_list { 
                let mut ast_root = $1?; //A implementação escolhida pelo nosso grupo foi a geração de código intermediário em duas passagens. A outra passagem ocorre na função main.rs (linha 46.)
                ast_root.generate_initial_code()?;
                Ok(ast_root)
        } |
        { Ok(ASTNode::None) } ;

item_list -> Result<ASTNode, ParsingError>:
        function item_list  {
                match $1? {
                        ASTNode::FunctionDeclaration(mut node) => {
                                let next_fn = Box::new($2?);
                                node.add_next_function(next_fn);
                                Ok(ASTNode::FunctionDeclaration(node))
                        }
                        _ => unreachable!(),
                }
        }|
        global_variable item_list { $1?;//Mecanismo de propagação de erros de parseamento.
                                    $2 } |
        function  { $1 }|
        global_variable { $1?;//Mecanismo de propagação de erros de parseamento.
                          Ok(ASTNode::None) };

global_variable -> Result<(), ParsingError>:
        type name_list ';' { let variable_type = $1?;
                             for temp in $2? {
                                let symbol = SymbolEntry::from_untyped_var(temp, variable_type.clone());
                                add_symbol(symbol)?;                                
                             } 
                             Ok(())
                             };

name_list -> Result<Vec<UntypedVar>, ParsingError>:
        identifier ',' name_list  {
                let identifier = $1?;
                let variable = UntypedVar::new(identifier.span()?, $lexer);
                let mut var_vec = vec![variable];
                var_vec.extend($3?);
                Ok(var_vec)
        } |
        identifier {
                let identifier = $1?;
                let variable = UntypedVar::new(identifier.span()?, $lexer);
                Ok(vec![variable])
        };

function -> Result<ASTNode, ParsingError>:
        identifier parameters "TK_OC_MAP" type function_body  {
                let identifier = $1?;
                let function_type = $4?;
                let name = $lexer.span_str(identifier.span()?).to_string();

                add_name_to_last_symbol_table(name.clone());

                let arguments = $2?;                
                let function_label = get_new_label();


                let entry = SymbolEntry::Function(FunctionSymbol::new(name, function_type, $span, $lexer, arguments, function_label));
                add_symbol(entry)?;
                let command = Box::new($5?);
                let node = FunctionDeclaration::new($span, command, identifier.span()?, $lexer)?;
                Ok(ASTNode::FunctionDeclaration(node))
        } ;

parameters-> Result<Option<Vec<SymbolEntry>>, ParsingError>:
        '(' scope_begin_function parameters_list ')' {
                let symbols = $3?;         
                Ok(Some(symbols))
        }  |
        '(' scope_begin_function ')' {                              
                                Ok(None) };

parameters_list->  Result<Vec<SymbolEntry>, ParsingError>:
        type identifier ',' parameters_list {
                let name = $lexer.span_str($2?.span()?).to_string();
                let variable = SymbolEntry::Variable(CommonAttrs::new(name, $1?, $span, $lexer));
                add_symbol(variable.clone())?;

                let mut list = vec![variable];
                list.extend($4?);
                Ok(list)
        }| 
        type identifier {
                let name = $lexer.span_str($2?.span()?).to_string();
                let variable = SymbolEntry::Variable(CommonAttrs::new(name, $1?, $span, $lexer));
                add_symbol(variable.clone())?;

                Ok(vec![variable])
        };

function_body-> Result<ASTNode, ParsingError>:
        '{' command_list scope_end '}' { $2 } |
        '{' '}' { Ok(ASTNode::None) };


command_block -> Result<ASTNode, ParsingError>:
        '{' scope_begin_inner command_list scope_end '}' { $3 } |
        '{' '}' { Ok(ASTNode::None) };

command_list -> Result<ASTNode, ParsingError>:
        command command_list{
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

command-> Result<ASTNode, ParsingError>:
        variable_declare ';' { $1 } | 
        assignment ';' { $1 } | 
        function_call ';'  { $1 }| 
        return ';'  { $1 }| 
        flow_ctrl ';'  { $1 }| 
        command_block ';'  { $1 };

variable_declare -> Result<ASTNode, ParsingError>:
        type variable_list {
                let auxiliar = $2?;
                let variable_type = $1?;
                for variable in auxiliar.variables {
                        let symbol = SymbolEntry::from_untyped_var(variable, variable_type.clone());
                        add_symbol(symbol)?;
                }
                let node = auxiliar.node.update_type(variable_type, $lexer)?;
                Ok(node)
        } ;

variable_list -> Result<LocalDeclrAux, ParsingError>:
        identifier "TK_OC_LE" literal ',' variable_list {
                let identifier = $1?;
                let mut auxiliar = $5?;

                let variable = UntypedVar::new(identifier.span()?, $lexer);
                let literal = Box::new($3?);

                auxiliar.variables.push(variable);
                let node = InitializedVariable::new($span, Box::new(identifier), literal, Some(Box::new(auxiliar.node)));
                auxiliar.node = ASTNode::InitializedVariable(node);
                Ok(auxiliar)
        } |
        identifier "TK_OC_LE" literal {
                let identifier = $1?;
                let literal = Box::new($3?);
                let variable = UntypedVar::new(identifier.span()?, $lexer);
                let node = ASTNode::InitializedVariable(InitializedVariable::new($span, Box::new(identifier), literal, None));
                Ok(LocalDeclrAux::new(vec![variable], node))
        } |
        identifier ',' variable_list {
                let identifier = $1?;
                let mut auxiliar = $3?;
                let variable = UntypedVar::new(identifier.span()?, $lexer);
                auxiliar.variables.push(variable);
                Ok(auxiliar)
        } |
        identifier {
                let identifier = $1?;
                let variables = vec![UntypedVar::new(identifier.span()?, $lexer)];
                Ok(LocalDeclrAux::with_vars(variables))
        } ;

assignment-> Result<ASTNode, ParsingError>:
        verified_identifier '=' expression { 
                let identifier = Box::new($1?);
                let expression = Box::new($3?);
                let node = AssignmentCommand::new($span, identifier, expression, $lexer)?;
                Ok(ASTNode::AssignmentCommand(node))
        };

function_call -> Result<ASTNode, ParsingError>:
         identifier '(' arguments_list ')' {
                let expression = Box::new($3?);
                let identifier = $1?;
                check_declaration(&identifier, $lexer, UsageType::FunctionCall)?;
                let node = FunctionCallCommand::new($span, expression, Box::new(identifier))?;
                Ok(ASTNode::FunctionCallCommand(node))
        } |
        identifier '(' ')'  {
                let identifier = $1?;
                check_declaration(&identifier, $lexer, UsageType::FunctionCall)?;
                let node = FunctionCallCommand::new($span, Box::new(ASTNode::None), Box::new(identifier))?;
                Ok(ASTNode::FunctionCallCommand(node))
        } ;

arguments_list-> Result<ASTNode, ParsingError>:
        expression ',' arguments_list {
                let expression = $1?;
                let next = Box::new($3?);
                let node = expression.add_next(next)?;
                Ok(node)
        } |
        expression  { $1 } ;

return ->       Result<ASTNode, ParsingError>:
        "TK_PR_RETURN" expression  {
                let expression = Box::new($2?);
                let node = ReturnCommand::new($span, expression);
                Ok(ASTNode::ReturnCommand(node))
        } ;

flow_ctrl ->    Result<ASTNode, ParsingError>:
        if       { $1 } |
        while    { $1 } ;

if->    Result<ASTNode, ParsingError>:
        "TK_PR_IF" '(' expression ')' command_block {
            let expression = Box::new($3?);
            let true_command = Box::new($5?);
            let node = IfCommand::new($span, expression, true_command, Box::new(ASTNode::None));
            Ok(ASTNode::IfCommand(node))
         }|  
        "TK_PR_IF" '(' expression ')' command_block "TK_PR_ELSE" command_block {
            let expression = Box::new($3?);
            let true_command = Box::new($5?);
            let false_command = Box::new($7?);
            let node = IfCommand::new($span, expression, true_command, false_command);
            Ok(ASTNode::IfCommand(node))
        };
while-> Result<ASTNode, ParsingError>:
        "TK_PR_WHILE"  '(' expression ')' command_block{
            let expression = Box::new($3?);
            let command = Box::new($5?);
            let node = WhileCommand::new($span, expression, command);
            Ok(ASTNode::WhileCommand(node))
        }; 

expression -> Result<ASTNode, ParsingError>:
        expression "TK_OC_OR" expression2 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::OrExpression(node))
        } |
        expression2  { $1 } ;

expression2-> Result<ASTNode, ParsingError>:
        expression2 "TK_OC_AND" expression3 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::AndExpression(node))
        } |
        expression3  { $1 } ;

expression3->  Result<ASTNode, ParsingError>:
        expression3 "TK_OC_EQ" expression4 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::EqualExpression(node))
        } |
        expression3 "TK_OC_NE" expression4 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::NotEqualExpression(node))
        } |
        expression4  { $1 } ;

expression4-> Result<ASTNode, ParsingError>:
        expression4 '<' expression5 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::LessThanExpression(node))
         } |
        expression4 '>' expression5 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::GreaterThanExpression(node))
         } |
        expression4 "TK_OC_LE" expression5 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::LessEqualExpression(node))
         } |
        expression4 "TK_OC_GE" expression5 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::GreaterEqualExpression(node))
         } |
        expression5  { $1 } ;

expression5->  Result<ASTNode, ParsingError>:
        expression5 '+' expression6 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::AdditionExpression(node))
         } |
        expression5 '-' expression6 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::SubtractionExpression(node))
         } |
        expression6  { $1 } ;

expression6-> Result<ASTNode, ParsingError>:
        expression6 '*' expression7 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::MultiplyExpression(node))
        } |
        expression6 '/' expression7 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::DivisionExpression(node))
        } |
        expression6 '%' expression7 {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinaryOperation::new($span, child_left, child_right)?;
                Ok(ASTNode::ModExpression(node))
        } |
        expression7  { $1 } ;

expression7-> Result<ASTNode, ParsingError>:
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

expression8-> Result<ASTNode, ParsingError>:
        '(' expression ')'    { $2 } |
        operand         { $1 } ;

operand -> Result<ASTNode, ParsingError>:
        verified_identifier      { $1 } |        
        function_call   { $1 } |  
        literal         { $1 } ;

verified_identifier -> Result<ASTNode, ParsingError>:
        identifier {
                let identifier = $1?;
                check_declaration(&identifier, $lexer, UsageType::Variable)?;
                Ok(identifier)
        } ;

identifier -> Result<ASTNode, ParsingError>:
        "TK_IDENTIFICADOR" { 
                                let ((line,_), _) = $lexer.line_col($span);   
                                Ok(ASTNode::Identifier(Identifier::new($span,line,$lexer)))} ;

literal -> Result<ASTNode, ParsingError>:
        "TK_LIT_INT"      { 
                                let ((line,_), _) = $lexer.line_col($span);
                                let literal_entry = SymbolEntry::from_lit_span($span, $lexer);
                                add_symbol(literal_entry)?;
                                Ok(ASTNode::LiteralInt(LiteralInt::new($span,line,$lexer)))
                 } |
        "TK_LIT_FLOAT"    { 
                                let ((line,_), _) = $lexer.line_col($span);   
                                let literal_entry = SymbolEntry::from_lit_span($span, $lexer);
                                add_symbol(literal_entry)?;                             
                                Ok(ASTNode::LiteralFloat(LiteralFloat::new($span,line))) } |       
        "TK_LIT_TRUE"     { 
                                let ((line,_), _) = $lexer.line_col($span); 
                                let literal_entry = SymbolEntry::from_lit_span($span, $lexer);
                                add_symbol(literal_entry)?;   
                                Ok(ASTNode::LiteralBool(LiteralBool::new($span,line))) } |
        "TK_LIT_FALSE"    { 
                                let ((line,_), _) = $lexer.line_col($span);  
                                let literal_entry = SymbolEntry::from_lit_span($span, $lexer);
                                add_symbol(literal_entry)?; 
                                Ok(ASTNode::LiteralBool(LiteralBool::new($span,line))) } ;

type-> Result<Type, ParsingError>:
        "TK_PR_INT" { Ok(Type::INT) } |
        "TK_PR_FLOAT" { Ok(Type::FLOAT) } |
        "TK_PR_BOOL" { Ok(Type::BOOL) } ;

scope_begin_function -> Result<(), ParsingError>:
        {
                new_scope(ScopeType::Inner);
                Ok(())
        };

scope_begin_inner -> Result<(), ParsingError>:
        {
                new_scope(ScopeType::Function);
                Ok(())
        };

scope_end -> Result<(), ParsingError>:
        {
                end_scope();
                Ok(())
        };
%%

use etapa5::{ast::{
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
        Identifier},
        errors::ParsingError,
        add_symbol,
        new_scope,
        end_scope,
        get_new_label,
        add_name_to_last_symbol_table,      
        symbol_table::{
                SymbolEntry,
                CommonAttrs,
                FunctionSymbol,  
                ScopeType,  
        },
        untyped::{
                check_declaration,
                UsageType,               
                UntypedVar,
                LocalDeclrAux,
        },
        type_enum::{
                Type,
        }};
