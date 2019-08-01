mod prelude;
mod tokenizer;
#[macro_use]
#[allow(dead_code)]
mod fancyterm;
mod parser;
mod reporter;
mod functional;
mod ast;


use fancyterm::*;
use prelude::*;
use reporter::*;
use tokenizer::*;

use std::env::args;
use std::fs::File;
use std::io;
use std::io::prelude::*;

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

fn main() {
    let mut args = args().collect::<Vec<String>>();
    args.push("/home/sv-97/GitHub/Stackl/stackl/examples/src2.stackl".to_string()); // only for debugging
    let filename = &args[1];
    let source = read_file(filename);

    let source = Source::new(filename.clone(), source);
    let logger = Logger::new(source.clone());
    let tokens = Tokenizer::new(source.clone(), Some(logger));
    for tok
    
     in tokens {
        println!(
            "{}{} {:?}",
            tok,
            colored!(":", params!(Modifier::Faint)),
            source.from_span(&tok.span)
        );
    }
}
