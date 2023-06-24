use std::io::{self, BufRead, Write};

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

lrlex_mod!("scanner.l");
lrpar_mod!("parser.y");

fn main() {
    // Get the `LexerDef` for the `calc` language.
    let lexerdef = scanner_l::lexerdef();
    let stdin = io::stdin();
    loop {}
}
