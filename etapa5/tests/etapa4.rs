#[cfg(test)]
mod test {
    use etapa5::{clear_stack, new_scope};
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;

    lrlex_mod!("scanner.l");
    lrpar_mod!("parser.y");

    #[test]
    fn test_etapa4() {
        let inputs = std::fs::read_dir("./tests/E4").expect("Esse diretório deveria existir");

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
            let (tree, _errors) = parser_y::parse(&lexer);
            let expected = get_expected(&input);

            println!("Arquivo:{:#?}", input_file_name);

            match tree.unwrap() {
                Ok(_) => {
                    assert!(
                        expected == 0,
                        "Parseamento completo de um arquivo incorreto. Nome: {}. Código de erro esperado: {}",
                        input_file_name,
                        expected
                    );
                    clear_stack();
                }
                Err(err) => {
                    assert_eq!(
                        expected,
                        err.to_err_code(),
                        "Erro ao parsear arquivo {}. Erro esperado: {}. Erro obtido: {}. Mensagem de erro: {}",
                        input_file_name,
                        expected,
                        err.to_err_code(),
                        err,
                    );
                    clear_stack();
                }
            }
        }
    }

    fn get_expected(input: &str) -> u8 {
        let first = *input.split('\n').collect::<Vec<_>>().first().unwrap();

        if !first.contains("//") {
            return 0;
        }

        match &first[2..] {
            "ERR_UNDECLARED" => 10,
            "ERR_DECLARED" => 11,
            "ERR_VARIABLE" => 20,
            "ERR_FUNCTION" => 21,
            _ => 100,
        }
    }
}
