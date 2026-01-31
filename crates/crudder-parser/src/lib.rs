//! Parser for the Crudder DSL.
//!
//! Implements a recursive descent parser that produces an AST from tokens.

use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use crudder_ast::{Annotation, Dto, Field, Method, PrimitiveType, Schema, Service, TypeRef};
use crudder_lexer::{lex, LexerError, SpannedToken, Token};
use thiserror::Error;

/// Parser error types.
#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("lexer error: {message}")]
    LexerError { span: Range<usize>, message: String },

    #[error("unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        span: Range<usize>,
        expected: String,
        found: String,
    },

    #[error("unexpected end of input: expected {expected}")]
    UnexpectedEof { expected: String },

    #[error("{message}")]
    Custom { span: Range<usize>, message: String },
}

impl From<LexerError> for ParseError {
    fn from(err: LexerError) -> Self {
        ParseError::LexerError {
            span: err.span,
            message: err.message,
        }
    }
}

impl ParseError {
    /// Returns the span of this error, if available.
    pub fn span(&self) -> Option<Range<usize>> {
        match self {
            ParseError::LexerError { span, .. } => Some(span.clone()),
            ParseError::UnexpectedToken { span, .. } => Some(span.clone()),
            ParseError::UnexpectedEof { .. } => None,
            ParseError::Custom { span, .. } => Some(span.clone()),
        }
    }

    /// Prints a pretty error report using ariadne.
    pub fn report(&self, filename: &str, source: &str) {
        let offset = self.span().map(|s| s.start).unwrap_or(0);
        let mut builder =
            Report::build(ReportKind::Error, filename, offset).with_message(format!("{}", self));

        if let Some(span) = self.span() {
            let label = match self {
                ParseError::LexerError { message, .. } => message.clone(),
                ParseError::UnexpectedToken {
                    expected, found, ..
                } => {
                    format!("expected {expected}, found {found}")
                }
                ParseError::Custom { message, .. } => message.clone(),
                ParseError::UnexpectedEof { .. } => unreachable!(),
            };
            builder = builder.with_label(
                Label::new((filename, span))
                    .with_message(label)
                    .with_color(Color::Red),
            );
        }

        let report = builder.finish();
        report
            .print((filename, Source::from(source)))
            .expect("failed to print error report");
    }
}

/// Parser for the Crudder DSL.
pub struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
    #[allow(dead_code)]
    source: String,
}

impl Parser {
    /// Creates a new parser for the given source.
    pub fn new(source: &str) -> Result<Self, ParseError> {
        let tokens = lex(source)?;
        Ok(Self {
            tokens,
            pos: 0,
            source: source.to_string(),
        })
    }

    /// Returns the current token, if any.
    fn current(&self) -> Option<&SpannedToken> {
        self.tokens.get(self.pos)
    }

    /// Returns the current token's token type, if any.
    fn current_token(&self) -> Option<&Token> {
        self.current().map(|st| &st.token)
    }

    /// Advances to the next token.
    fn advance(&mut self) {
        self.pos += 1;
    }

    /// Checks if we've reached the end of input.
    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Returns the span of the last token, or 0..0 if empty.
    #[allow(dead_code)]
    fn last_span(&self) -> Range<usize> {
        if self.pos > 0 {
            self.tokens[self.pos - 1].span.clone()
        } else if !self.tokens.is_empty() {
            self.tokens[0].span.clone()
        } else {
            0..0
        }
    }

    /// Expects and consumes a specific token.
    fn expect(&mut self, expected: Token) -> Result<SpannedToken, ParseError> {
        match self.current() {
            Some(st) if std::mem::discriminant(&st.token) == std::mem::discriminant(&expected) => {
                let st = st.clone();
                self.advance();
                Ok(st)
            }
            Some(st) => Err(ParseError::UnexpectedToken {
                span: st.span.clone(),
                expected: format!("{expected:?}"),
                found: format!("{:?}", st.token),
            }),
            None => Err(ParseError::UnexpectedEof {
                expected: format!("{expected:?}"),
            }),
        }
    }

    /// Expects and consumes an identifier, returning its name.
    fn expect_ident(&mut self) -> Result<(String, Range<usize>), ParseError> {
        match self.current() {
            Some(SpannedToken {
                token: Token::Ident(name),
                span,
            }) => {
                let name = name.clone();
                let span = span.clone();
                self.advance();
                Ok((name, span))
            }
            Some(st) => Err(ParseError::UnexpectedToken {
                span: st.span.clone(),
                expected: "identifier".to_string(),
                found: format!("{:?}", st.token),
            }),
            None => Err(ParseError::UnexpectedEof {
                expected: "identifier".to_string(),
            }),
        }
    }

    /// Parses the entire schema.
    pub fn parse(&mut self) -> Result<Schema, ParseError> {
        let mut schema = Schema::new();

        while !self.at_end() {
            // Parse any leading annotations
            let annotations = self.parse_annotations()?;

            match self.current_token() {
                Some(Token::Dto) => {
                    schema.dtos.push(self.parse_dto(annotations)?);
                }
                Some(Token::Service) => {
                    schema.services.push(self.parse_service(annotations)?);
                }
                Some(Token::At) => {
                    // More annotations - continue collecting
                    continue;
                }
                Some(_) => {
                    let st = self.current().unwrap();
                    return Err(ParseError::UnexpectedToken {
                        span: st.span.clone(),
                        expected: "dto or service".to_string(),
                        found: format!("{:?}", st.token),
                    });
                }
                None => break,
            }
        }

        Ok(schema)
    }

    /// Parses zero or more annotations.
    fn parse_annotations(&mut self) -> Result<Vec<Annotation>, ParseError> {
        let mut annotations = Vec::new();
        while matches!(self.current_token(), Some(Token::At)) {
            annotations.push(self.parse_annotation()?);
        }
        Ok(annotations)
    }

    /// Parses a DTO definition.
    fn parse_dto(&mut self, annotations: Vec<Annotation>) -> Result<Dto, ParseError> {
        let start = annotations
            .first()
            .and_then(|a| a.span.map(|(s, _)| s))
            .unwrap_or_else(|| self.current().map(|t| t.span.start).unwrap_or(0));

        self.expect(Token::Dto)?;
        let (name, _) = self.expect_ident()?;
        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();
        while !matches!(self.current_token(), Some(Token::RBrace) | None) {
            fields.push(self.parse_field()?);
        }

        let end_token = self.expect(Token::RBrace)?;

        Ok(Dto {
            name,
            fields,
            annotations,
            span: Some((start, end_token.span.end)),
        })
    }

    /// Parses a field definition (with optional annotations).
    fn parse_field(&mut self) -> Result<Field, ParseError> {
        // Parse field annotations first
        let annotations = self.parse_annotations()?;

        let start = annotations
            .first()
            .and_then(|a| a.span.map(|(s, _)| s))
            .unwrap_or_else(|| self.current().map(|t| t.span.start).unwrap_or(0));

        let (name, _) = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let (ty, end) = self.parse_type()?;

        Ok(Field {
            name,
            ty,
            annotations,
            span: Some((start, end)),
        })
    }

    /// Parses a type reference.
    fn parse_type(&mut self) -> Result<(TypeRef, usize), ParseError> {
        // Parse the base type (possibly with array prefix)
        let (base_type, mut end) = self.parse_type_without_optional()?;

        // Check for optional suffix - this applies to the whole type
        if matches!(self.current_token(), Some(Token::Question)) {
            end = self.current().unwrap().span.end;
            self.advance();
            return Ok((TypeRef::Optional(Box::new(base_type)), end));
        }

        Ok((base_type, end))
    }

    /// Parses a type without the optional suffix.
    fn parse_type_without_optional(&mut self) -> Result<(TypeRef, usize), ParseError> {
        // Check for array prefix []
        if matches!(self.current_token(), Some(Token::LBracket)) {
            let _start = self.current().unwrap().span.start;
            self.advance();
            self.expect(Token::RBracket)?;
            // Recursively parse the element type (without optional suffix)
            let (inner, end) = self.parse_type_without_optional()?;
            return Ok((TypeRef::Array(Box::new(inner)), end));
        }

        // Parse base type (primitive or named)
        self.parse_base_type()
    }

    /// Parses a base type (primitive or named).
    fn parse_base_type(&mut self) -> Result<(TypeRef, usize), ParseError> {
        match self.current() {
            Some(st) => {
                let span_end = st.span.end;
                let ty = match &st.token {
                    Token::String => TypeRef::Primitive(PrimitiveType::String),
                    Token::Int => TypeRef::Primitive(PrimitiveType::Int),
                    Token::Float => TypeRef::Primitive(PrimitiveType::Float),
                    Token::Bool => TypeRef::Primitive(PrimitiveType::Bool),
                    Token::Uuid => TypeRef::Primitive(PrimitiveType::Uuid),
                    Token::Timestamp => TypeRef::Primitive(PrimitiveType::Timestamp),
                    Token::Bytes => TypeRef::Primitive(PrimitiveType::Bytes),
                    Token::Ident(name) => TypeRef::Named(name.clone()),
                    other => {
                        return Err(ParseError::UnexpectedToken {
                            span: st.span.clone(),
                            expected: "type".to_string(),
                            found: format!("{other:?}"),
                        });
                    }
                };
                self.advance();
                Ok((ty, span_end))
            }
            None => Err(ParseError::UnexpectedEof {
                expected: "type".to_string(),
            }),
        }
    }

    /// Parses a service definition.
    fn parse_service(&mut self, annotations: Vec<Annotation>) -> Result<Service, ParseError> {
        let start = annotations
            .first()
            .and_then(|a| a.span.map(|(s, _)| s))
            .unwrap_or_else(|| self.current().map(|t| t.span.start).unwrap_or(0));

        self.expect(Token::Service)?;
        let (name, _) = self.expect_ident()?;
        self.expect(Token::LBrace)?;

        let mut methods = Vec::new();
        while !matches!(self.current_token(), Some(Token::RBrace) | None) {
            methods.push(self.parse_method()?);
        }

        let end_token = self.expect(Token::RBrace)?;

        Ok(Service {
            name,
            methods,
            annotations,
            span: Some((start, end_token.span.end)),
        })
    }

    /// Parses a method definition.
    fn parse_method(&mut self) -> Result<Method, ParseError> {
        // Parse annotations first
        let annotations = self.parse_annotations()?;

        let start = annotations
            .first()
            .and_then(|a| a.span.map(|(s, _)| s))
            .unwrap_or_else(|| self.current().map(|t| t.span.start).unwrap_or(0));

        let (name, _) = self.expect_ident()?;
        self.expect(Token::LParen)?;
        let (input, _) = self.expect_ident()?;
        self.expect(Token::RParen)?;
        self.expect(Token::Arrow)?;

        // Parse return type (void or DTO name)
        let (output, end) = if matches!(self.current_token(), Some(Token::Void)) {
            let end = self.current().unwrap().span.end;
            self.advance();
            (None, end)
        } else {
            let (name, span) = self.expect_ident()?;
            (Some(name), span.end)
        };

        Ok(Method {
            name,
            input,
            output,
            annotations,
            span: Some((start, end)),
        })
    }

    /// Parses an annotation. Supports both `@name` and `@name(args)` forms.
    fn parse_annotation(&mut self) -> Result<Annotation, ParseError> {
        let start = self.current().map(|t| t.span.start).unwrap_or(0);
        self.expect(Token::At)?;
        let (name, name_span) = self.expect_ident()?;

        // Check if there are parentheses (optional for annotations like @primary, @auto)
        if matches!(self.current_token(), Some(Token::LParen)) {
            self.advance();

            let mut args = Vec::new();
            loop {
                match self.current() {
                    Some(SpannedToken {
                        token: Token::StringLiteral(s),
                        ..
                    }) => {
                        args.push(s.clone());
                        self.advance();
                    }
                    Some(SpannedToken {
                        token: Token::Ident(s),
                        ..
                    }) => {
                        // Allow bare identifiers as annotation args (e.g., POST, GET)
                        args.push(s.clone());
                        self.advance();
                    }
                    _ => break,
                }

                if matches!(self.current_token(), Some(Token::Comma)) {
                    self.advance();
                } else {
                    break;
                }
            }

            let end_token = self.expect(Token::RParen)?;

            Ok(Annotation {
                name,
                args,
                span: Some((start, end_token.span.end)),
            })
        } else {
            // No parentheses - annotation without arguments (e.g., @primary, @auto)
            Ok(Annotation {
                name,
                args: vec![],
                span: Some((start, name_span.end)),
            })
        }
    }
}

/// Convenience function to parse a source string into a schema.
pub fn parse(source: &str) -> Result<Schema, ParseError> {
    let mut parser = Parser::new(source)?;
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty() {
        let schema = parse("").unwrap();
        assert!(schema.dtos.is_empty());
        assert!(schema.services.is_empty());
    }

    #[test]
    fn test_parse_simple_dto() {
        let schema = parse(
            r#"
            dto User {
                name: string
                age: int
            }
        "#,
        )
        .unwrap();

        assert_eq!(schema.dtos.len(), 1);
        let dto = &schema.dtos[0];
        assert_eq!(dto.name, "User");
        assert_eq!(dto.fields.len(), 2);
        assert_eq!(dto.fields[0].name, "name");
        assert_eq!(dto.fields[0].ty, TypeRef::Primitive(PrimitiveType::String));
        assert_eq!(dto.fields[1].name, "age");
        assert_eq!(dto.fields[1].ty, TypeRef::Primitive(PrimitiveType::Int));
    }

    #[test]
    fn test_parse_optional_type() {
        let schema = parse(
            r#"
            dto User {
                nickname: string?
            }
        "#,
        )
        .unwrap();

        let dto = &schema.dtos[0];
        assert_eq!(
            dto.fields[0].ty,
            TypeRef::Optional(Box::new(TypeRef::Primitive(PrimitiveType::String)))
        );
    }

    #[test]
    fn test_parse_array_type() {
        let schema = parse(
            r#"
            dto User {
                tags: []string
            }
        "#,
        )
        .unwrap();

        let dto = &schema.dtos[0];
        assert_eq!(
            dto.fields[0].ty,
            TypeRef::Array(Box::new(TypeRef::Primitive(PrimitiveType::String)))
        );
    }

    #[test]
    fn test_parse_optional_array() {
        let schema = parse(
            r#"
            dto User {
                tags: []string?
            }
        "#,
        )
        .unwrap();

        let dto = &schema.dtos[0];
        assert_eq!(
            dto.fields[0].ty,
            TypeRef::Optional(Box::new(TypeRef::Array(Box::new(TypeRef::Primitive(
                PrimitiveType::String
            )))))
        );
    }

    #[test]
    fn test_parse_named_type() {
        let schema = parse(
            r#"
            dto Address {
                street: string
            }
            dto User {
                address: Address
            }
        "#,
        )
        .unwrap();

        assert_eq!(schema.dtos.len(), 2);
        let user = &schema.dtos[1];
        assert_eq!(user.fields[0].ty, TypeRef::Named("Address".to_string()));
    }

    #[test]
    fn test_parse_service() {
        let schema = parse(
            r#"
            dto Request {}
            dto Response {}
            service MyService {
                DoSomething(Request) -> Response
            }
        "#,
        )
        .unwrap();

        assert_eq!(schema.services.len(), 1);
        let service = &schema.services[0];
        assert_eq!(service.name, "MyService");
        assert_eq!(service.methods.len(), 1);
        assert_eq!(service.methods[0].name, "DoSomething");
        assert_eq!(service.methods[0].input, "Request");
        assert_eq!(service.methods[0].output, Some("Response".to_string()));
    }

    #[test]
    fn test_parse_void_return() {
        let schema = parse(
            r#"
            dto Request {}
            service MyService {
                DoSomething(Request) -> void
            }
        "#,
        )
        .unwrap();

        assert_eq!(schema.services[0].methods[0].output, None);
    }

    #[test]
    fn test_parse_annotation() {
        let schema = parse(
            r#"
            dto Request {}
            dto Response {}
            service MyService {
                @rest(POST, "/users")
                CreateUser(Request) -> Response
            }
        "#,
        )
        .unwrap();

        let method = &schema.services[0].methods[0];
        assert_eq!(method.annotations.len(), 1);
        assert_eq!(method.annotations[0].name, "rest");
        assert_eq!(method.annotations[0].args, vec!["POST", "/users"]);
    }

    #[test]
    fn test_parse_annotation_with_string_literals() {
        let schema = parse(
            r#"
            dto Request {}
            dto Response {}
            service MyService {
                @rest("GET", "/users/{id}")
                GetUser(Request) -> Response
            }
        "#,
        )
        .unwrap();

        let method = &schema.services[0].methods[0];
        assert_eq!(method.annotations[0].args, vec!["GET", "/users/{id}"]);
    }

    #[test]
    fn test_parse_full_example() {
        let schema = parse(
            r#"
            dto CreateUserRequest {
                name: string
                email: string
            }

            dto UserResponse {
                id: uuid
                name: string
                email: string
            }

            dto GetUserRequest {
                id: uuid
            }

            service UserService {
                @rest(POST, "/users")
                CreateUser(CreateUserRequest) -> UserResponse

                @rest(GET, "/users/{id}")
                GetUser(GetUserRequest) -> UserResponse
            }
        "#,
        )
        .unwrap();

        assert_eq!(schema.dtos.len(), 3);
        assert_eq!(schema.services.len(), 1);
        assert_eq!(schema.services[0].methods.len(), 2);
    }

    #[test]
    fn test_error_unexpected_token() {
        let result = parse("dto 123 {}");
        assert!(result.is_err());
    }

    // New tests for database annotations

    #[test]
    fn test_parse_dto_with_table_annotation() {
        let schema = parse(
            r#"
            @table("todos")
            dto Todo {
                id: uuid
                title: string
            }
        "#,
        )
        .unwrap();

        let dto = &schema.dtos[0];
        assert_eq!(dto.annotations.len(), 1);
        assert_eq!(dto.table_name(), Some("todos"));
    }

    #[test]
    fn test_parse_field_with_annotations() {
        let schema = parse(
            r#"
            @table("todos")
            dto Todo {
                @primary @auto
                id: uuid

                title: string

                @column("created_at")
                createdAt: timestamp
            }
        "#,
        )
        .unwrap();

        let dto = &schema.dtos[0];
        assert_eq!(dto.fields.len(), 3);

        // id field
        let id_field = &dto.fields[0];
        assert!(id_field.is_primary());
        assert!(id_field.is_auto());

        // createdAt field
        let created_at = &dto.fields[2];
        assert_eq!(created_at.column_name(), "created_at");
    }

    #[test]
    fn test_parse_service_with_storage() {
        let schema = parse(
            r#"
            dto Request {}
            dto Response {}

            @storage(postgres)
            service MyService {
                @rest(GET, "/items")
                ListItems(Request) -> Response
            }
        "#,
        )
        .unwrap();

        let service = &schema.services[0];
        assert_eq!(
            service.storage_type(),
            Some(crudder_ast::StorageType::Postgres)
        );
    }

    #[test]
    fn test_parse_references_annotation() {
        let schema = parse(
            r#"
            @table("users")
            dto User {
                @primary @auto
                id: uuid
                name: string
            }

            @table("todos")
            dto Todo {
                @primary @auto
                id: uuid

                @references(User)
                userId: uuid

                title: string
            }
        "#,
        )
        .unwrap();

        let todo = &schema.dtos[1];
        let user_id_field = &todo.fields[1];
        assert_eq!(user_id_field.references(), Some("User"));
    }
}
