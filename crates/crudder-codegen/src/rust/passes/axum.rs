//! Axum web framework pass for Rust.

use crudder_ast::{Method, Schema, Service};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::pass::{GenerationContext, Pass};
use crate::rust::base::to_snake_case;
use crate::CodegenError;

/// Axum pass that generates web handlers and routing.
///
/// This pass creates handler files for each service with stub implementations,
/// a lib.rs with router setup, and optionally a Cargo.toml.
///
/// It depends on `rust-base` and `serde` passes (Axum handlers need serde for Json<T>).
pub struct AxumPass;

impl Pass for AxumPass {
    fn name(&self) -> &'static str {
        "axum"
    }

    fn depends_on(&self) -> &[&'static str] {
        &["rust-base", "serde"]
    }

    fn run(&self, schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
        // Generate handlers for each service
        for service in &schema.services {
            let handlers_code = generate_service_handlers(service, schema)?;
            let filename = format!("src/{}.rs", to_snake_case(&service.name));
            ctx.set_file(&filename, handlers_code);
        }

        // Generate lib.rs with router
        let lib_code = generate_lib(&schema.services)?;
        ctx.set_file("src/lib.rs", lib_code);

        // Add Axum dependencies
        ctx.set_metadata("cargo:dep:axum", "\"0.8\"");
        ctx.set_metadata(
            "cargo:dep:tokio",
            "{ version = \"1\", features = [\"full\"] }",
        );
        ctx.set_metadata(
            "cargo:dep:uuid",
            "{ version = \"1\", features = [\"serde\", \"v4\"] }",
        );
        ctx.set_metadata(
            "cargo:dep:chrono",
            "{ version = \"0.4\", features = [\"serde\"] }",
        );

        // Signal that Axum is available
        ctx.set_metadata("has:axum", "true");

        Ok(())
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

    let routes: Vec<TokenStream> = service.methods.iter().map(generate_route).collect();

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

    crate::rust::format_rust(tokens)
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

    let method_fn = match http_method.as_str() {
        "POST" => quote! { post },
        "PUT" => quote! { put },
        "DELETE" => quote! { delete },
        _ => quote! { get },
    };

    quote! {
        .route(#axum_path, #method_fn(#fn_name))
    }
}

/// Generates the lib.rs file.
fn generate_lib(services: &[Service]) -> Result<String, CodegenError> {
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

    crate::rust::format_rust(tokens)
}

/// Gets the HTTP method from annotation or infers from method name.
fn get_http_method(method: &Method) -> String {
    // Check for @rest annotation
    if let Some(ann) = method.annotations.iter().find(|a| a.name == "rest") {
        if let Some(http_method) = ann.args.first() {
            return http_method.trim().to_uppercase();
        }
    }

    // Infer from method name
    let name = &method.name;
    let lower = name.to_lowercase();
    if lower.starts_with("create") || lower.starts_with("add") {
        "POST".to_string()
    } else if lower.starts_with("update") || lower.starts_with("set") {
        "PUT".to_string()
    } else if lower.starts_with("delete") || lower.starts_with("remove") {
        "DELETE".to_string()
    } else {
        "GET".to_string()
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

#[cfg(test)]
mod tests {
    use super::*;
    use crudder_ast::Annotation;

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
}
