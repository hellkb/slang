use std::{
    fs::File,
    io::Write,
    iter::{Enumerate, Peekable},
    str::Chars,
};
#[derive(Debug,Copy, Clone)]
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
    pub fn get_inner_string_val(&self) -> &str {
        match self {
            Token::Ident(s, _) => s,
            Token::NumLit(s, _) => s,
            Token::StrLit(s, _) => s,
            Token::Punct(s, _) => s,
        }
    }
    pub fn get_inner_values(&self) -> (String, Span) {
        let (s,span) = match self {
            Token::Ident(s, span) => (s,span),
            Token::NumLit(s, span) => (s,span),
            Token::StrLit(s, span) => (s,span),
            Token::Punct(s, span) => (s,span),
        };
        (s.to_string(), *span)
    }

    pub fn is_ident(&self, s: Option<&str>) -> bool {
        matches!(self, Self::Ident(_, _) if 
            matches!(s,Some(x) if x == self.get_inner_string_val())|| s.is_none())
    }
    pub fn is_numlit(&self, s: Option<&str>) -> bool {
        matches!(self, Self::NumLit(_, _) if 
            matches!(s,Some(x) if x == self.get_inner_string_val())|| s.is_none())
    }
    pub fn is_punct(&self, s: Option<&str>) -> bool {
        matches!(self, Self::Punct(_, _) if 
            matches!(s,Some(x) if x == self.get_inner_string_val())|| s.is_none())
    }

    
}

/// compare two Tokens, ignoring Span
impl std::cmp::PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other) 
            && self.get_inner_string_val() == other.get_inner_string_val()
    }
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
    
}
impl Lexer {
    pub fn new(src: String, name: String) -> Self {
        Self {
            src,
            name,
            puncts: vec![
                "=", "==", ">=", "<=", "!=", "+", "-", "*", "/", "(", ")", "{", "}", "[", "]", ";",
                "->", "//", "&&", "||", "=>", ":", "%", ",", "//",
            ],
            //keywords: vec!["let", "if", "else", "match", "fn", "struct", "enum"],
        }
    }

    pub fn tokens(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut start_column: usize;
        let mut comment= false;
        for (row, line) in self.src.lines().enumerate() {
            let mut line = line.chars().enumerate().peekable();
            comment = false;
            while let Some((col, c)) = line.peek() {
                start_column = *col;
                if comment {
                    line.next();
                    continue;
                }
                match c {
                    '0'..='9' => tokens.push(get_num_lit(&mut line, row, start_column)),

                    '"' => tokens.push(get_str_lit(&mut line, row, start_column)),
                    //'/' => tokens.push(get_punc(&mut line, row, &self.puncts)),
                    _ if c.is_whitespace() => {
                        line.next();
                    }

                    _ if c.is_alphabetic() || *c == '_' => {
                        tokens.push(get_ident(&mut line, row, start_column))
                    }
                    _ if c.is_ascii_punctuation() => {
                        let p = get_punc(&mut line, row, &self.puncts);
                        if p.get_inner_string_val() == "//" {
                            comment = true;
                        }  else { 
                            tokens.push(p);
                        }
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
