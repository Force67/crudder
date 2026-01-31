//! Lexer for the Crudder DSL using logos.

use logos::Logos;

/// Token types for the Crudder language.
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token {
    // Keywords
    #[token("dto")]
    Dto,

    #[token("service")]
    Service,

    #[token("void")]
    Void,

    // Primitive types
    #[token("string")]
    String,

    #[token("int")]
    Int,

    #[token("float")]
    Float,

    #[token("bool")]
    Bool,

    #[token("uuid")]
    Uuid,

    #[token("timestamp")]
    Timestamp,

    #[token("bytes")]
    Bytes,

    // Symbols
    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("->")]
    Arrow,

    #[token(":")]
    Colon,

    #[token("?")]
    Question,

    #[token("@")]
    At,

    #[token(",")]
    Comma,

    // String literals (for annotation arguments)
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // Remove quotes and unescape
        s[1..s.len()-1].to_string()
    })]
    StringLiteral(std::string::String),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(std::string::String),

    // Comments (skipped)
    #[regex(r"//[^\n]*", logos::skip)]
    Comment,
}

/// A token with its span information.
#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: std::ops::Range<usize>,
}

/// Lexer wrapper that provides iteration over spanned tokens.
pub struct Lexer<'source> {
    inner: logos::Lexer<'source, Token>,
}

impl<'source> Lexer<'source> {
    /// Creates a new lexer for the given source.
    pub fn new(source: &'source str) -> Self {
        Self {
            inner: Token::lexer(source),
        }
    }

    /// Returns the source string being lexed.
    pub fn source(&self) -> &'source str {
        self.inner.source()
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Result<SpannedToken, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner.next()?;
        let span = self.inner.span();

        match token {
            Ok(token) => Some(Ok(SpannedToken { token, span })),
            Err(_) => {
                let slice = &self.inner.source()[span.clone()];
                Some(Err(LexerError {
                    span,
                    message: format!("unexpected character: {:?}", slice),
                }))
            }
        }
    }
}

/// An error that occurred during lexing.
#[derive(Debug, Clone, PartialEq)]
pub struct LexerError {
    pub span: std::ops::Range<usize>,
    pub message: std::string::String,
}

/// Convenience function to lex a source string into a vector of tokens.
pub fn lex(source: &str) -> Result<Vec<SpannedToken>, LexerError> {
    Lexer::new(source).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keywords() {
        let tokens: Vec<_> = lex("dto service void").unwrap();
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].token, Token::Dto);
        assert_eq!(tokens[1].token, Token::Service);
        assert_eq!(tokens[2].token, Token::Void);
    }

    #[test]
    fn test_primitives() {
        let tokens: Vec<_> = lex("string int float bool uuid timestamp bytes").unwrap();
        assert_eq!(tokens.len(), 7);
        assert_eq!(tokens[0].token, Token::String);
        assert_eq!(tokens[1].token, Token::Int);
        assert_eq!(tokens[2].token, Token::Float);
        assert_eq!(tokens[3].token, Token::Bool);
        assert_eq!(tokens[4].token, Token::Uuid);
        assert_eq!(tokens[5].token, Token::Timestamp);
        assert_eq!(tokens[6].token, Token::Bytes);
    }

    #[test]
    fn test_symbols() {
        let tokens: Vec<_> = lex("{ } ( ) [ ] -> : ? @ ,").unwrap();
        assert_eq!(tokens.len(), 11);
        assert_eq!(tokens[0].token, Token::LBrace);
        assert_eq!(tokens[1].token, Token::RBrace);
        assert_eq!(tokens[2].token, Token::LParen);
        assert_eq!(tokens[3].token, Token::RParen);
        assert_eq!(tokens[4].token, Token::LBracket);
        assert_eq!(tokens[5].token, Token::RBracket);
        assert_eq!(tokens[6].token, Token::Arrow);
        assert_eq!(tokens[7].token, Token::Colon);
        assert_eq!(tokens[8].token, Token::Question);
        assert_eq!(tokens[9].token, Token::At);
        assert_eq!(tokens[10].token, Token::Comma);
    }

    #[test]
    fn test_identifier() {
        let tokens: Vec<_> = lex("CreateUserRequest _private foo123").unwrap();
        assert_eq!(tokens.len(), 3);
        assert_eq!(
            tokens[0].token,
            Token::Ident("CreateUserRequest".to_string())
        );
        assert_eq!(tokens[1].token, Token::Ident("_private".to_string()));
        assert_eq!(tokens[2].token, Token::Ident("foo123".to_string()));
    }

    #[test]
    fn test_string_literal() {
        let tokens: Vec<_> = lex(r#""hello" "/users/{id}""#).unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token, Token::StringLiteral("hello".to_string()));
        assert_eq!(
            tokens[1].token,
            Token::StringLiteral("/users/{id}".to_string())
        );
    }

    #[test]
    fn test_comments_skipped() {
        let tokens: Vec<_> = lex("dto // this is a comment\nservice").unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].token, Token::Dto);
        assert_eq!(tokens[1].token, Token::Service);
    }

    #[test]
    fn test_full_dto() {
        let source = r#"
            dto User {
                name: string
                age: int?
            }
        "#;
        let tokens: Vec<_> = lex(source).unwrap();
        assert!(tokens.len() > 0);
        assert_eq!(tokens[0].token, Token::Dto);
    }

    #[test]
    fn test_annotation() {
        let tokens: Vec<_> = lex(r#"@rest("POST", "/users")"#).unwrap();
        assert_eq!(tokens[0].token, Token::At);
        assert_eq!(tokens[1].token, Token::Ident("rest".to_string()));
        assert_eq!(tokens[2].token, Token::LParen);
        assert_eq!(tokens[3].token, Token::StringLiteral("POST".to_string()));
        assert_eq!(tokens[4].token, Token::Comma);
        assert_eq!(tokens[5].token, Token::StringLiteral("/users".to_string()));
        assert_eq!(tokens[6].token, Token::RParen);
    }
}
