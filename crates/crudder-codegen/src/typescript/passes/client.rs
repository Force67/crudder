//! API client pass for TypeScript.

use crudder_ast::{Method, Schema};

use crate::pass::{GenerationContext, Pass};
use crate::typescript::base::{to_camel_case, to_snake_case};
use crate::CodegenError;

/// Client pass that generates a fetch-based API client SDK.
///
/// This pass creates a `client.ts` file with a typed API client for all services.
/// It depends on the `typescript-base` pass to have run first.
pub struct ClientPass;

impl Pass for ClientPass {
    fn name(&self) -> &'static str {
        "client"
    }

    fn depends_on(&self) -> &[&'static str] {
        &["typescript-base"]
    }

    fn run(&self, schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
        let client_code = generate_client(schema);
        ctx.set_file("client.ts", client_code);

        // Mark that client is available
        ctx.set_metadata("has:client", "true");

        Ok(())
    }
}

/// Generates a client SDK for the schema.
fn generate_client(schema: &Schema) -> String {
    let mut code = String::from(
        r#"// Generated API client from Crudder schema

import type * as Types from "./types";

export interface ClientOptions {
  baseUrl: string;
  fetch?: typeof fetch;
  /** Auth token to include in requests. Format: "<uuid>:<role1>,<role2>" */
  authToken?: string;
}

export class ApiClient {
  private baseUrl: string;
  private fetch: typeof fetch;
  private authToken?: string;

  constructor(options: ClientOptions) {
    this.baseUrl = options.baseUrl.replace(/\/$/, "");
    this.fetch = options.fetch ?? globalThis.fetch;
    this.authToken = options.authToken;
  }

  /**
   * Sets the auth token for subsequent requests.
   */
  setAuthToken(token: string | undefined): void {
    this.authToken = token;
  }

  private async request<T>(
    method: string,
    path: string,
    body?: unknown
  ): Promise<T> {
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
    };

    if (this.authToken) {
      headers["Authorization"] = `Bearer ${this.authToken}`;
    }

    const response = await this.fetch(`${this.baseUrl}${path}`, {
      method,
      headers,
      body: body ? JSON.stringify(body) : undefined,
    });

    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }

    return response.json();
  }

"#,
    );

    for service in &schema.services {
        for method in &service.methods {
            code.push_str(&generate_client_method(method));
        }
    }

    code.push_str("}\n");
    code
}

/// Generates a client method.
fn generate_client_method(method: &Method) -> String {
    let fn_name = to_camel_case(&method.name);
    let http_method = get_http_method(method);
    let path = get_path(method);
    let input_type = &method.input;

    let return_type = method
        .output
        .as_ref()
        .map(|o| format!("Types.{}", o))
        .unwrap_or_else(|| "void".to_string());

    // Check if path has parameters
    let has_path_param = path.contains('{');

    if has_path_param {
        // Extract path param name
        let param_name = path
            .split('{')
            .nth(1)
            .and_then(|s| s.split('}').next())
            .unwrap_or("id");

        if http_method == "GET" || http_method == "DELETE" {
            format!(
                r#"  async {fn_name}({param_name}: string): Promise<{return_type}> {{
    return this.request("{http_method}", `{path_template}`);
  }}

"#,
                fn_name = fn_name,
                param_name = param_name,
                return_type = return_type,
                http_method = http_method,
                path_template =
                    path.replace(&format!("{{{}}}", param_name), &format!("${{{}}}", param_name))
            )
        } else {
            format!(
                r#"  async {fn_name}({param_name}: string, request: Types.{input_type}): Promise<{return_type}> {{
    return this.request("{http_method}", `{path_template}`, request);
  }}

"#,
                fn_name = fn_name,
                param_name = param_name,
                input_type = input_type,
                return_type = return_type,
                http_method = http_method,
                path_template =
                    path.replace(&format!("{{{}}}", param_name), &format!("${{{}}}", param_name))
            )
        }
    } else if http_method == "GET" {
        format!(
            r#"  async {fn_name}(request: Types.{input_type}): Promise<{return_type}> {{
    const params = new URLSearchParams(request as Record<string, string>);
    return this.request("{http_method}", `{path}?${{params}}`);
  }}

"#,
            fn_name = fn_name,
            input_type = input_type,
            return_type = return_type,
            http_method = http_method,
            path = path
        )
    } else {
        format!(
            r#"  async {fn_name}(request: Types.{input_type}): Promise<{return_type}> {{
    return this.request("{http_method}", "{path}", request);
  }}

"#,
            fn_name = fn_name,
            input_type = input_type,
            return_type = return_type,
            http_method = http_method,
            path = path
        )
    }
}

/// Gets the HTTP method from annotation or infers from method name.
fn get_http_method(method: &Method) -> &str {
    if let Some(ann) = method.annotations.iter().find(|a| a.name == "rest") {
        if let Some(http_method) = ann.args.first() {
            return http_method.as_str();
        }
    }

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
    if let Some(ann) = method.annotations.iter().find(|a| a.name == "rest") {
        if let Some(path) = ann.args.get(1) {
            return path.clone();
        }
    }

    format!("/{}", to_snake_case(&method.name))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crudder_ast::Annotation;

    #[test]
    fn test_generate_client_method_get_with_path_param() {
        let method = Method {
            name: "GetUser".to_string(),
            input: "Empty".to_string(),
            output: Some("User".to_string()),
            annotations: vec![Annotation {
                name: "rest".to_string(),
                args: vec!["GET".to_string(), "/users/{id}".to_string()],
                span: None,
            }],
            span: None,
        };

        let code = generate_client_method(&method);
        assert!(code.contains("async getUser(id: string)"));
        assert!(code.contains("Promise<Types.User>"));
        assert!(code.contains("`/users/${id}`"));
    }

    #[test]
    fn test_generate_client_method_post() {
        let method = Method {
            name: "CreateUser".to_string(),
            input: "CreateUserRequest".to_string(),
            output: Some("User".to_string()),
            annotations: vec![Annotation {
                name: "rest".to_string(),
                args: vec!["POST".to_string(), "/users".to_string()],
                span: None,
            }],
            span: None,
        };

        let code = generate_client_method(&method);
        assert!(code.contains("async createUser(request: Types.CreateUserRequest)"));
        assert!(code.contains("Promise<Types.User>"));
        assert!(code.contains("\"/users\""));
    }
}
