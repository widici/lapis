#[macro_use]
extern crate log;
mod args;
use eval::{env::Enviroment, eval::Evaluator};
use lexer::Lexer;
use parser::Parser;
use resolver::Resolver;
use span::file::set_file_path;
use std::fs::File;
use std::io::Read;

fn main() {
    env_logger::init();

    let args = args::parse();

    set_file_path(args.path.to_str().unwrap().to_owned());

    let mut file = match File::open(args.path) {
        Ok(file) => file,
        Err(_) => unimplemented!(),
    };

    let mut contents = String::new();
    if file.read_to_string(&mut contents).is_err() {
        unimplemented!()
    }
    let chars: Vec<char> = contents.chars().collect();

    let mut lexer = Lexer::new(chars);
    let tokens = lexer.get_tokens();
    info!("Lexed: {:?}", tokens);
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse();
    info!("Parsed: {:?}", stmts);

    let resolver = Resolver::new();
    let side_table = resolver.resolve(stmts.clone());
    info!("Resolved side-table: {:?}", side_table);

    let env = Enviroment::new(side_table);
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
