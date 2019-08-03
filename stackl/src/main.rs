mod prelude;
mod tokenizer;
#[macro_use]
#[allow(dead_code)]
mod fancyterm;
mod ast;
mod dot_translator;
mod functional;
mod parser;
mod reporter;
mod shell;

use fancyterm::*;
use parser::*;
use prelude::*;
use reporter::*;
use shell::Shell;
use tokenizer::*;

use std::env::args;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

/// Read a the contents of a file relative to the project's root directory
fn read_file(filename: &str) -> String {
    std::path::Path::new(filename);
    let path = &filename;
    let mut file = File::open(path).expect("Failed to open file");
    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Failed to read file");
    source
}

fn write_file(filename: &str, data: String) {
    std::path::Path::new(filename);
    let path = &filename;
    let mut file = File::create(path).expect("Failed to open file");
    file.write_all(data.as_bytes())
        .expect("Failed to write to file");
}

fn handle_file(filename: &str) {
    let source = read_file(filename);

    let source = Rc::new(Source::new(filename.to_string(), source));
    let logger = Rc::new(Logger::new(Rc::clone(&source)));
    let tokens = Tokenizer::new(Rc::clone(&source), Some(Rc::clone(&logger)));
    let mut parser = Parser::new(tokens, Rc::clone(&source), None); // Some(Rc::clone(&logger))
    let root = parser.parse();
    let dot_txt = dot_translator::visit(&root);
    write_file("/home/sv-97/GitHub/Stackl/stackl/out/test.dot", dot_txt);
    // println!("{:?}", parser.parse());
}

fn handle_repl() {
    let mut repl = Shell::new("Stackl");
    repl.set_ansi(params!(), params!(Color::Blue, Modifier::Faint), params!());

    let source = Rc::new(Source::new("Interactive".to_string(), "".to_string()));
    let logger = Rc::new(Logger::new(Rc::clone(&source)));
    let mut tokens = Tokenizer::new(Rc::clone(&source), Some(Rc::clone(&logger)));
    let mut parser = Parser::new(tokens, Rc::clone(&source), None); // Some(Rc::clone(&logger))
    for input in 0.. {
        repl.set_seperators(
            &format!("In[{:?}]: ", input),
            &format!("Out[{:?}]: ", input),
        );
        let text = repl.read();
        let source = Rc::new(Source::new("Interactive".to_string(), text));
        tokens = parser.get_tokenizer();
        tokens.set_source(Rc::clone(&source));
        tokens.reset();
        parser = Parser::new(tokens, Rc::clone(&source), None); // Some(Rc::clone(&logger))
        let root = parser.parse();
        let dot_txt = dot_translator::visit(&root);
        write_file("/home/sv-97/GitHub/Stackl/stackl/out/test.dot", dot_txt);
        repl.writeln(format!("{:?}", root));
    }
}

fn main() {
    let mut args = args().collect::<Vec<String>>();
    args.push("/home/sv-97/GitHub/Stackl/stackl/examples/src2.stackl".to_string()); // only for debugging
    let filename = &args.get(1);

    match filename {
        Some(name) => handle_file(name),
        _ => handle_repl(),
    }
}
