pub mod lexer;
use lexer::*;

fn main() {
    let mut lexer = Lexer::new("1234 1+2 1.2e2 1e3 .e452");

    loop {
        match lexer.next_token() {
            Ok(TokenType::EOF) => break,
            Ok(tok) => println!("{0:?}", tok),
            Err(err) => println!("{0:?}", err),
        }
    }
}
