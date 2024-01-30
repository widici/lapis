#[macro_use]
extern crate log;
mod args;
use error::{
    impl_error_handling, report_errors, Error,
    ErrorKind::{FileNotFound, FileNotRead},
    ErrorLocation,
};
use eval::{env::Enviroment, eval::Evaluator};
use lexer::Lexer;
use parser::Parser;
use resolver::Resolver;
use span::file::set_file_path;
use std::fs::File;
use std::io::Read;

fn main() {
    env_logger::init();

    let mut file_reader = FileReader {};
    let chars = file_reader.read_from_file();

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
    report_errors();
}

struct FileReader {}

impl_error_handling!(FileReader, ErrorLocation::Initial);

impl FileReader {
    pub(crate) fn read_from_file(&mut self) -> Vec<char> {
        let args = args::parse();
        let path = args.path.to_str().unwrap().to_owned();

        set_file_path(path.clone());

        let mut file = match File::open(args.path) {
            Ok(file) => file,
            Err(e) => {
                self.add_error(FileNotFound {
                    path,
                    msg: e.to_string(),
                });
                report_errors();
                unreachable!()
            }
        };

        let mut contents = String::new();
        if let Err(e) = file.read_to_string(&mut contents) {
            self.add_error(FileNotRead {
                path,
                msg: e.to_string(),
            });
            report_errors();
            unreachable!()
        }
        contents.chars().collect()
    }
}
