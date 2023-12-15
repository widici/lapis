#[macro_use]
extern crate log;

use eval::{eval::Evaluator, env::Enviroment};
use resolver::Resolver;
use lexer::Lexer;
use parser::Parser;

const FILE_PATH: &str = "./test.unamned";

fn main() {
    env_logger::init();
    
    let chars: Vec<char> = match std::fs::read_to_string(FILE_PATH) {
        Ok(src) => src.chars().collect(),
        Err(_) => unimplemented!(),
    };

    let mut lexer = Lexer::new(chars);
    let tokens = lexer.get_tokens();
    info!("Lexed: {:?}", tokens);
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse();
    info!("Parsed: {:?}", stmts);

    let mut resolver = Resolver::new();
    let _ = resolver.resolve(stmts.clone());
    info!("Resolved side-table: {:?}", resolver.side_table);

    let env = Enviroment::new(resolver);
    let mut evaluator = Evaluator::new(env);

    let _ = evaluator.evaluate(stmts);
}

/*
fn repl() {
    let resolver = Resolver::new();
    let env = Enviroment::new();
    let mut evaluator = Evaluator::new(resolver, env);
    loop {
        print!(">> ");
        let mut input: String = String::new();
        std::io::stdout().flush().expect("Failed to flush output!");
        std::io::stdin().read_line(&mut input).expect("Failed to read line!");
        let input = input.trim_end();

        let mut lexer = Lexer::new(input.chars().collect());
        let tokens = lexer.get_tokens();
        info!("Lexed: {:?}", tokens);
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse();
        info!("Parsed: {:?}", stmts);

        let _ = evaluator.evaluate(stmts);
    }
}
*/
