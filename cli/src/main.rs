//use
use std::env;

use parser::parser::Parser;
fn main() -> std::io::Result<()> {
    let fname = match env::args().nth(1) {
        Some(f) => f,
        None => "srccode/1.slang".into(),
    };
    let name = fname.clone();

    let src = std::fs::read_to_string(fname)?;

    let mut parser = Parser::new(src, name);
    let ast = parser.parse();

    println!("{:#?}", ast);
    println!("hallo");
    Ok(())
}
