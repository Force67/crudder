//! Rust code generator targeting the Axum framework.

use crudder_ast::{Dto, Field, Method, PrimitiveType, Schema, Service, TypeRef};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::{CodeGenerator, CodegenError, GeneratedFile, GeneratedFiles};

/// Rust code generator for Axum.
pub struct RustGenerator {
    /// Whether to generate a Cargo.toml file.
    pub generate_cargo_toml: bool,
}

impl RustGenerator {
    /// Creates a new Rust generator with default settings.
    pub fn new() -> Self {
        Self {
            generate_cargo_toml: true,
        }
    }
}

impl Default for RustGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenerator for RustGenerator {
    fn generate(&self, schema: &Schema) -> Result<GeneratedFiles, CodegenError> {
        let mut files = GeneratedFiles::new();

        // Generate types module
        let types_code = generate_types(&schema.dtos);
        files.add(GeneratedFile::new("src/types.rs", types_code));

        // Generate handlers for each service
        for service in &schema.services {
            let handlers_code = generate_service_handlers(service, schema)?;
            let filename = format!("src/{}.rs", to_snake_case(&service.name));
            files.add(GeneratedFile::new(filename, handlers_code));
        }

        // Generate main lib.rs
        let lib_code = generate_lib(&schema.services);
        files.add(GeneratedFile::new("src/lib.rs", lib_code));

        // Generate Cargo.toml if requested
        if self.generate_cargo_toml {
            files.add(GeneratedFile::new("Cargo.toml", generate_cargo_toml()));
        }

        Ok(files)
    }

    fn name(&self) -> &'static str {
        "rust"
    }
}

/// Generates the types module with all DTOs.
fn generate_types(dtos: &[Dto]) -> String {
    let dto_structs: Vec<TokenStream> = dtos.iter().map(generate_dto_struct).collect();

    let tokens = quote! {
        //! Generated types from Crudder schema.

        use serde::{Deserialize, Serialize};
        use uuid::Uuid;
        use chrono::{DateTime, Utc};

        #(#dto_structs)*
    };

    prettyplease::unparse(&syn::parse2(tokens).expect("generated invalid Rust code"))
}

/// Generates a struct for a DTO.
fn generate_dto_struct(dto: &Dto) -> TokenStream {
    let name = format_ident!("{}", dto.name);
    let fields: Vec<TokenStream> = dto.fields.iter().map(generate_field).collect();

    quote! {
        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub struct #name {
            #(#fields)*
        }
    }
}

/// Generates a struct field.
fn generate_field(field: &Field) -> TokenStream {
    let name = format_ident!("{}", to_snake_case(&field.name));
    let ty = type_ref_to_rust(&field.ty);

    // Use serde rename if the field name changes
    let original_name = &field.name;
    let snake_name = to_snake_case(original_name);

    if snake_name != *original_name {
        quote! {
            #[serde(rename = #original_name)]
            pub #name: #ty,
        }
    } else {
        quote! {
            pub #name: #ty,
        }
    }
}

/// Converts a TypeRef to a Rust type.
fn type_ref_to_rust(ty: &TypeRef) -> TokenStream {
    match ty {
        TypeRef::Primitive(p) => primitive_to_rust(p),
        TypeRef::Array(inner) => {
            let inner_ty = type_ref_to_rust(inner);
            quote! { Vec<#inner_ty> }
        }
        TypeRef::Optional(inner) => {
            let inner_ty = type_ref_to_rust(inner);
            quote! { Option<#inner_ty> }
        }
        TypeRef::Named(name) => {
            let ident = format_ident!("{}", name);
            quote! { #ident }
        }
    }
}

/// Converts a primitive type to Rust.
fn primitive_to_rust(p: &PrimitiveType) -> TokenStream {
    match p {
        PrimitiveType::String => quote! { String },
        PrimitiveType::Int => quote! { i64 },
        PrimitiveType::Float => quote! { f64 },
        PrimitiveType::Bool => quote! { bool },
        PrimitiveType::Uuid => quote! { Uuid },
        PrimitiveType::Cuid2 => quote! { String },
        PrimitiveType::Timestamp => quote! { DateTime<Utc> },
        PrimitiveType::Bytes => quote! { Vec<u8> },
    }
}

/// Generates handlers for a service.
fn generate_service_handlers(service: &Service, schema: &Schema) -> Result<String, CodegenError> {
    let service_name = &service.name;
    let module_name = to_snake_case(service_name);
    let router_fn_name = format_ident!("{}_router", module_name);

    let handlers: Vec<TokenStream> = service
        .methods
        .iter()
        .map(|m| generate_handler(m, schema))
        .collect::<Result<_, _>>()?;

    let routes: Vec<TokenStream> = service
        .methods
        .iter()
        .map(|m| generate_route(m))
        .collect();

    let tokens = quote! {
        //! Generated handlers for #service_name.

        use axum::{
            extract::{Json, Path, Query},
            routing::{get, post, put, delete},
            Router,
        };
        use uuid::Uuid;

        use crate::types::*;

        /// Error type for handlers.
        pub type Error = axum::response::ErrorResponse;

        #(#handlers)*

        /// Creates a router for #service_name.
        pub fn #router_fn_name() -> Router {
            Router::new()
                #(#routes)*
        }
    };

    Ok(prettyplease::unparse(
        &syn::parse2(tokens).expect("generated invalid Rust code"),
    ))
}

/// Generates a handler function for a method.
fn generate_handler(method: &Method, _schema: &Schema) -> Result<TokenStream, CodegenError> {
    let fn_name = format_ident!("{}", to_snake_case(&method.name));
    let input_type = format_ident!("{}", method.input);

    // Determine HTTP method from annotation or infer from name
    let http_method = get_http_method(method);

    // Check if this is a path-based method (has {id} in path)
    let path = get_path(method);
    let has_path_param = path.contains('{');

    // Generate handler signature based on HTTP method and path params
    let (params, body) = if has_path_param {
        if http_method == "GET" || http_method == "DELETE" {
            // GET/DELETE with path param - use Path extractor
            (
                quote! { Path(id): Path<Uuid> },
                quote! {
                    // TODO: Implement handler logic
                    todo!("Implement {} handler", stringify!(#fn_name))
                },
            )
        } else {
            // POST/PUT with path param - use both Path and Json
            (
                quote! { Path(id): Path<Uuid>, Json(req): Json<#input_type> },
                quote! {
                    // TODO: Implement handler logic
                    let _ = (id, req);
                    todo!("Implement {} handler", stringify!(#fn_name))
                },
            )
        }
    } else if http_method == "GET" {
        // GET without path param - use Query
        (
            quote! { Query(req): Query<#input_type> },
            quote! {
                // TODO: Implement handler logic
                let _ = req;
                todo!("Implement {} handler", stringify!(#fn_name))
            },
        )
    } else {
        // POST/PUT/DELETE without path param - use Json
        (
            quote! { Json(req): Json<#input_type> },
            quote! {
                // TODO: Implement handler logic
                let _ = req;
                todo!("Implement {} handler", stringify!(#fn_name))
            },
        )
    };

    let return_type = if let Some(output) = &method.output {
        let output_type = format_ident!("{}", output);
        quote! { Result<Json<#output_type>, Error> }
    } else {
        quote! { Result<(), Error> }
    };

    Ok(quote! {
        pub async fn #fn_name(#params) -> #return_type {
            #body
        }
    })
}

/// Generates a route registration for a method.
fn generate_route(method: &Method) -> TokenStream {
    let fn_name = format_ident!("{}", to_snake_case(&method.name));
    let path = get_path(method);
    let http_method = get_http_method(method);

    // Convert {id} to :id for Axum
    let axum_path = path.replace('{', ":").replace('}', "");

    let method_fn = match http_method {
        "POST" => quote! { post },
        "PUT" => quote! { put },
        "DELETE" => quote! { delete },
        _ => quote! { get },
    };

    quote! {
        .route(#axum_path, #method_fn(#fn_name))
    }
}

/// Gets the HTTP method from annotation or infers from method name.
fn get_http_method(method: &Method) -> &str {
    // Check for @rest annotation
    if let Some(ann) = method.annotations.iter().find(|a| a.name == "rest") {
        if let Some(http_method) = ann.args.first() {
            return http_method.as_str();
        }
    }

    // Infer from method name
    let name = &method.name;
    if name.starts_with("Create") || name.starts_with("Add") {
        "POST"
    } else if name.starts_with("Update") || name.starts_with("Set") {
        "PUT"
    } else if name.starts_with("Delete") || name.starts_with("Remove") {
        "DELETE"
    } else {
        "GET"
    }
}

/// Gets the path from annotation or generates a default.
fn get_path(method: &Method) -> String {
    // Check for @rest annotation
    if let Some(ann) = method.annotations.iter().find(|a| a.name == "rest") {
        if let Some(path) = ann.args.get(1) {
            return path.clone();
        }
    }

    // Generate default path from method name
    format!("/{}", to_snake_case(&method.name))
}

/// Generates the lib.rs file.
fn generate_lib(services: &[Service]) -> String {
    let modules: Vec<TokenStream> = services
        .iter()
        .map(|s| {
            let name = format_ident!("{}", to_snake_case(&s.name));
            quote! { pub mod #name; }
        })
        .collect();

    let router_calls: Vec<TokenStream> = services
        .iter()
        .map(|s| {
            let mod_name = format_ident!("{}", to_snake_case(&s.name));
            let router_fn = format_ident!("{}_router", to_snake_case(&s.name));
            quote! { .merge(#mod_name::#router_fn()) }
        })
        .collect();

    let tokens = quote! {
        //! Generated Crudder service.

        pub mod types;
        #(#modules)*

        use axum::Router;

        /// Creates the main router with all services.
        pub fn router() -> Router {
            Router::new()
                #(#router_calls)*
        }
    };

    prettyplease::unparse(&syn::parse2(tokens).expect("generated invalid Rust code"))
}

/// Generates a Cargo.toml for the generated crate.
fn generate_cargo_toml() -> String {
    r#"[package]
name = "generated-service"
version = "0.1.0"
edition = "2021"

[dependencies]
axum = "0.7"
serde = { version = "1", features = ["derive"] }
serde_json = "1"
uuid = { version = "1", features = ["serde", "v4"] }
chrono = { version = "0.4", features = ["serde"] }
tokio = { version = "1", features = ["full"] }
"#
    .to_string()
}

/// Converts a PascalCase or camelCase string to snake_case.
fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crudder_ast::{Annotation, Field};

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("CreateUser"), "create_user");
        assert_eq!(to_snake_case("UserService"), "user_service");
        assert_eq!(to_snake_case("ID"), "i_d");
        assert_eq!(to_snake_case("getUserByID"), "get_user_by_i_d");
    }

    #[test]
    fn test_get_http_method_from_annotation() {
        let method = Method {
            name: "Foo".to_string(),
            input: "Request".to_string(),
            output: Some("Response".to_string()),
            annotations: vec![Annotation {
                name: "rest".to_string(),
                args: vec!["POST".to_string(), "/foo".to_string()],
                span: None,
            }],
            span: None,
        };
        assert_eq!(get_http_method(&method), "POST");
    }

    #[test]
    fn test_get_http_method_inferred() {
        let method = Method {
            name: "CreateUser".to_string(),
            input: "Request".to_string(),
            output: Some("Response".to_string()),
            annotations: vec![],
            span: None,
        };
        assert_eq!(get_http_method(&method), "POST");

        let method = Method {
            name: "GetUser".to_string(),
            input: "Request".to_string(),
            output: Some("Response".to_string()),
            annotations: vec![],
            span: None,
        };
        assert_eq!(get_http_method(&method), "GET");
    }

    #[test]
    fn test_generate_dto() {
        let dto = Dto {
            name: "User".to_string(),
            fields: vec![
                Field {
                    name: "id".to_string(),
                    ty: TypeRef::Primitive(PrimitiveType::Uuid),
                    annotations: vec![],
                    index: None,
                    span: None,
                },
                Field {
                    name: "name".to_string(),
                    ty: TypeRef::Primitive(PrimitiveType::String),
                    annotations: vec![],
                    index: None,
                    span: None,
                },
            ],
            annotations: vec![],
            span: None,
        };

        let code = generate_types(&[dto]);
        assert!(code.contains("pub struct User"));
        assert!(code.contains("pub id: Uuid"));
        assert!(code.contains("pub name: String"));
    }
}
