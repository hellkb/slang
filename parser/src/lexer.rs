use std::{
    fs::File,
    io::Write,
    iter::{Enumerate, Peekable},
    str::Chars,
};
#[derive(Debug, Clone)]
pub struct Span {
    pub row: u32,
    pub column: u32,
}
impl Span {
    pub fn new(row: usize, column: usize) -> Self {
        Self {
            row: row as u32 + 1,
            column: column as u32 + 1,
        }
    }
}
type LineIter<'a> = Peekable<Enumerate<Chars<'a>>>;
#[derive(Debug)]
pub enum Token {
    Ident(String, Span),
    NumLit(String, Span),
    StrLit(String, Span),
    Punct(String, Span),
}

impl Token {
    pub fn get_punct(&self) -> String {
        match self {
            Token::Punct(p, _) => p.into(),
            _ => String::new(),
        }
    }
}

pub struct Lexer {
    src: String,
    name: String,
    puncts: Vec<&'static str>,
    keywords: Vec<&'static str>,
}
impl Lexer {
    pub fn new(src: String, name: String) -> Self {
        Self {
            src,
            name,
            puncts: vec![
                "=", "==", ">=", "<=", "!=", "+", "-", "*", "/", "(", ")", "{", "}", "[", "]", ";",
                "->", "//", "&&", "||", "=>", ":", "%", ",",
            ],
            keywords: vec!["let", "if", "else", "match", "fn", "struct", "enum"],
        }
    }

    pub fn tokens(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut start_column: usize;
        for (row, line) in self.src.lines().enumerate() {
            let mut line = line.chars().enumerate().peekable();

            while let Some((col, c)) = line.peek() {
                start_column = *col;
                match c {
                    '0'..='9' => tokens.push(get_num_lit(&mut line, row, start_column)),

                    '"' => tokens.push(get_str_lit(&mut line, row, start_column)),
                    // '/' => tokens.push(get_punc(&mut line, row, &self.puncts)),
                    _ if c.is_whitespace() => {
                        line.next();
                    }

                    _ if c.is_alphabetic() || *c == '_' => {
                        tokens.push(get_ident(&mut line, row, start_column))
                    }
                    _ if c.is_ascii_punctuation() => {
                        tokens.push(get_punc(&mut line, row, &self.puncts))
                    }
                    _ => {
                        //Error?
                        line.next();
                    }
                }
            }
        }
        self.save(&tokens);
        tokens
    }
    fn save(&self, toks: &Vec<Token>) {
        let mut file = self.name.clone();
        file.push_str(".tok");

        let mut file = File::create(file).unwrap();
        for item in toks {
            let s = format!("{:?}\n", item);
            file.write_all(s.as_bytes()).unwrap();
        }
    }
}

fn get_ident(line: &mut LineIter, row: usize, column: usize) -> Token {
    let mut zstring: String = String::default();

    while let Some(c) = line
        .next_if(|c| c.1.is_alphanumeric() || c.1 == '_')
        .map(|c| c.1)
    {
        zstring.push(c);
    }

    Token::Ident(zstring, Span::new(row, column))
}
fn get_num_lit(line: &mut LineIter, row: usize, column: usize) -> Token {
    let mut lit: String = String::default();

    while let Some(c) = line
        .next_if(|c| c.1.is_numeric() || c.1 == '_')
        .map(|c| c.1)
    {
        lit.push(c);
    }

    Token::NumLit(lit, Span::new(row, column))
}

fn get_str_lit(line: &mut LineIter, row: usize, column: usize) -> Token {
    line.next(); // skip "
    let lit: String = line.map(|c| c.1).take_while(|c| *c != '"').collect();
    line.next();
    Token::StrLit(lit, Span::new(row, column))
}

fn get_punc(line: &mut LineIter, row: usize, puncts: &[&str]) -> Token {
    let (col, c) = line.next().unwrap();

    let mut zstring = c.to_string();
    zstring.push(line.peek().map_or('X', |c| c.1));
    if puncts.iter().any(|&v| v == zstring) {
        line.next();
        Token::Punct(zstring, Span::new(row, col))
    } else {
        Token::Punct(c.to_string(), Span::new(row, col))
    }
}