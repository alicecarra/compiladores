#[cfg(test)]
mod test {
    use etapa4::{clear_stack, new_scope};
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;

    lrlex_mod!("scanner.l");
    lrpar_mod!("parser.y");

    #[test]
    fn test_etapa3() {
        let inputs = std::fs::read_dir("./tests/E3").expect("Esse diretório deveria existir");

        for input in inputs.flatten() {
            let input_file_name = input
                .file_name()
                .to_str()
                .expect("Erro ao extrair o nome do arquivo.")
                .to_owned();

            let input = std::fs::read_to_string(input.path())
                .unwrap_or_else(|_| panic!("Erro de leitura no arquivo {}", input_file_name));

            new_scope();

            let lexerdef = scanner_l::lexerdef();
            let lexer = lexerdef.lexer(&input);
            let (tree, errors) = parser_y::parse(&lexer);

            //println!("Arquivo:{:#?}", input_file_name);

            let has_error_semantics = match tree {
                Some(tree_r) => match tree_r {
                    Ok(_) => false,
                    Err(_err) => {
                        //println!("Erro:{:#?}", err);
                        true
                    }
                },
                None => true,
            };

            let has_error_ast = !errors.is_empty();

            let has_error = has_error_ast || has_error_semantics;

            assert_eq!(input.contains("INCORRECT"), has_error);

            clear_stack();
        }
    }
}
