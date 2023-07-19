#[cfg(test)]
mod test {
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

            let lexerdef = scanner_l::lexerdef();
            let lexer = lexerdef.lexer(&input);
            let (_tree, errors) = parser_y::parse(&lexer);

            assert_eq!(input.contains("INCORRECT"), !errors.is_empty());

            // if let Some(tree) = _tree {
            //     if let Ok(tree) = tree {
            //         println!("\n == {} ==", input_file_name);
            //         println!("{input}");
            //         if !errors.is_empty() {
            //             for err in errors {
            //                 eprintln!("{}", err.pp(&lexer, &parser_y::token_epp));
            //             }
            //         }
            //         tree.print(&lexer);
            //     }
            // }
        }
    }
}
