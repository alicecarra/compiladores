use etapa5::new_scope;
use lrlex::lrlex_mod;
use lrpar::lrpar_mod;
use std::{
    io::{self, Write},
    process::ExitCode,
};

lrlex_mod!("scanner.l");
lrpar_mod!("parser.y");

fn main() -> ExitCode {
    io::stdout().flush().ok();
    let mut input = Vec::new();
    unsafe {
        //Workaround pra conseguir resgatar a entrada padrão do usuário da melhor forma.
        let mut buffer = [0 as libc::c_char; 500];
        let buffer_ptr = buffer.as_mut_ptr();
        while libc::fgets(buffer_ptr, 500, libc_stdhandle::stdin())
            != libc::PT_NULL as *mut libc::c_char
        {
            input.extend_from_slice(&buffer[..libc::strlen(buffer_ptr)]);
        }
    };
    let input = input.into_iter().map(|val| val as u8).collect::<Vec<_>>();
    let input =
        std::str::from_utf8(input.as_slice()).expect("Erro ao converter a entrada em string.");

    let lexerdef = scanner_l::lexerdef();
    let lexer = lexerdef.lexer(input);

    new_scope();

    let (tree, errors) = parser_y::parse(&lexer);
    if !errors.is_empty() {
        for err in errors {
            println!("{}", err.pp(&lexer, &parser_y::token_epp));
        }
        return ExitCode::from(1);
    }

    let tree = tree.unwrap();

    match tree {
        Ok(tree) => {
            let str = tree.node_to_string(&lexer);
            if !str.is_empty() {
                print!("{str}");
            }
            ExitCode::SUCCESS
        }
        Err(err) => {
            eprintln!("{err}");
            ExitCode::from(err.to_err_code())
        }
    }
}
