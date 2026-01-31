//! TypeScript code generator.

use crudder_ast::{AuthRequirement, Dto, Method, PrimitiveType, Schema, TypeRef};

use crate::{CodeGenerator, CodegenError, GeneratedFile, GeneratedFiles};

/// TypeScript code generator.
pub struct TypeScriptGenerator {
    /// Whether to generate Zod schemas for runtime validation.
    pub generate_zod: bool,
}

impl TypeScriptGenerator {
    /// Creates a new TypeScript generator with default settings.
    pub fn new() -> Self {
        Self { generate_zod: true }
    }
}

impl Default for TypeScriptGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenerator for TypeScriptGenerator {
    fn generate(&self, schema: &Schema) -> Result<GeneratedFiles, CodegenError> {
        let mut files = GeneratedFiles::new();

        // Generate types file
        let types_code = generate_types(&schema.dtos);
        files.add(GeneratedFile::new("types.ts", types_code));

        // Generate Zod schemas if enabled
        if self.generate_zod {
            let zod_code = generate_zod_schemas(&schema.dtos);
            files.add(GeneratedFile::new("schemas.ts", zod_code));
        }

        // Generate auth module
        let auth_code = generate_auth_module();
        files.add(GeneratedFile::new("auth.ts", auth_code));

        // Generate client SDK
        let client_code = generate_client(schema);
        files.add(GeneratedFile::new("client.ts", client_code));

        // Generate server handlers (Express-style)
        let handlers_code = generate_handlers(schema);
        files.add(GeneratedFile::new("handlers.ts", handlers_code));

        Ok(files)
    }

    fn name(&self) -> &'static str {
        "typescript"
    }
}

/// Generates the auth module with pluggable token validation.
fn generate_auth_module() -> String {
    r#"// Generated auth module with pluggable token validation.
//
// SECURITY: This module provides a TokenValidator interface that you MUST
// implement for production use. The included DevValidator is INSECURE.

import type { Request, Response, NextFunction } from "express";
import { createHmac, timingSafeEqual } from "crypto";

// ============================================================================
// Types
// ============================================================================

/**
 * Authenticated user extracted from a validated token.
 */
export interface AuthenticatedUser {
  id: string;
  roles: string[];
}

/**
 * Extends Express Request to include user.
 */
export interface AuthenticatedRequest extends Request {
  user?: AuthenticatedUser;
}

/**
 * Error thrown when authentication fails.
 */
export class AuthError extends Error {
  constructor(
    public statusCode: number,
    message: string
  ) {
    super(message);
    this.name = "AuthError";
  }

  static unauthorized(message = "Unauthorized"): AuthError {
    return new AuthError(401, message);
  }

  static forbidden(message = "Forbidden"): AuthError {
    return new AuthError(403, message);
  }
}

// ============================================================================
// TokenValidator interface - implement this for your auth system
// ============================================================================

/**
 * Interface for validating authentication tokens.
 *
 * Implement this to plug in your own authentication system:
 * - JWT validation (with jose, jsonwebtoken, etc.)
 * - Session-based auth (lookup in Redis/database)
 * - API key validation
 * - OAuth token introspection
 *
 * @example
 * ```typescript
 * import { jwtVerify } from "jose";
 *
 * class JwtValidator implements TokenValidator {
 *   constructor(private secret: Uint8Array) {}
 *
 *   async validate(token: string): Promise<AuthenticatedUser> {
 *     const { payload } = await jwtVerify(token, this.secret);
 *     return {
 *       id: payload.sub as string,
 *       roles: payload.roles as string[],
 *     };
 *   }
 * }
 * ```
 */
export interface TokenValidator {
  validate(token: string): Promise<AuthenticatedUser> | AuthenticatedUser;
}

// ============================================================================
// DevValidator - INSECURE, for development only
// ============================================================================

/**
 * Development-only token validator. DO NOT USE IN PRODUCTION.
 *
 * Accepts tokens in format: `{uuid}:{role1},{role2}`
 *
 * **Security Warning:** This validator trusts whatever the client sends:
 * - User ID is client-controlled (impersonation possible)
 * - Roles are client-controlled (privilege escalation possible)
 * - No cryptographic verification
 * - No expiration
 */
export class DevValidator implements TokenValidator {
  constructor() {
    console.warn("╔══════════════════════════════════════════════════════════════╗");
    console.warn("║  ⚠️  WARNING: Using DevValidator - INSECURE AUTH ⚠️            ║");
    console.warn("║                                                              ║");
    console.warn("║  Tokens can be forged by anyone. This is for development     ║");
    console.warn("║  only. For production, implement TokenValidator with JWT     ║");
    console.warn("║  or session-based auth.                                      ║");
    console.warn("╚══════════════════════════════════════════════════════════════╝");
  }

  validate(token: string): AuthenticatedUser {
    const [id, rolesStr] = token.split(":");

    if (!id || !/^[0-9a-f-]{36}$/i.test(id)) {
      throw AuthError.unauthorized("Invalid token format");
    }

    const roles = rolesStr ? rolesStr.split(",").filter(Boolean) : [];
    return { id, roles };
  }
}

// ============================================================================
// HmacValidator - Secure validator using HMAC-SHA256
// ============================================================================

/**
 * HMAC-based token validator using SHA-256.
 *
 * Token format: `{uuid}:{roles}:{timestamp}:{signature}`
 *
 * Provides:
 * - Cryptographic verification (tokens can't be forged without secret)
 * - Expiration (configurable TTL)
 * - Uses Node.js built-in crypto
 *
 * @example
 * ```typescript
 * const validator = new HmacValidator("your-secret-key-at-least-32-chars", 3600);
 *
 * // Generate a token
 * const token = validator.generateToken(userId, ["user", "admin"]);
 *
 * // Validate a token
 * const user = await validator.validate(token);
 * ```
 */
export class HmacValidator implements TokenValidator {
  constructor(
    private secret: string,
    private ttlSeconds: number = 3600
  ) {
    if (secret.length < 32) {
      console.warn("⚠️  Warning: HMAC secret should be at least 32 characters");
    }
  }

  /**
   * Generates a signed token for a user.
   */
  generateToken(userId: string, roles: string[]): string {
    const timestamp = Math.floor(Date.now() / 1000);
    const rolesStr = roles.join(",");
    const payload = `${userId}:${rolesStr}:${timestamp}`;
    const signature = this.sign(payload);
    return `${payload}:${signature}`;
  }

  private sign(payload: string): string {
    return createHmac("sha256", this.secret).update(payload).digest("hex");
  }

  validate(token: string): AuthenticatedUser {
    const parts = token.split(":");
    if (parts.length < 4) {
      throw AuthError.unauthorized("Invalid token format");
    }

    // Last part is signature, rest is payload
    const signature = parts.pop()!;
    const timestampStr = parts.pop()!;
    const rolesStr = parts.pop()!;
    const userId = parts.join(":"); // Handle UUIDs with colons (shouldn't happen but safe)

    const payload = `${userId}:${rolesStr}:${timestampStr}`;
    const expectedSig = this.sign(payload);

    // Constant-time comparison
    try {
      const sigBuffer = Buffer.from(signature, "hex");
      const expectedBuffer = Buffer.from(expectedSig, "hex");

      if (sigBuffer.length !== expectedBuffer.length || !timingSafeEqual(sigBuffer, expectedBuffer)) {
        throw AuthError.unauthorized("Invalid signature");
      }
    } catch {
      throw AuthError.unauthorized("Invalid signature");
    }

    // Check expiration
    if (this.ttlSeconds > 0) {
      const timestamp = parseInt(timestampStr, 10);
      const now = Math.floor(Date.now() / 1000);

      if (isNaN(timestamp) || now > timestamp + this.ttlSeconds) {
        throw AuthError.unauthorized("Token expired");
      }
    }

    // Validate UUID
    if (!/^[0-9a-f-]{36}$/i.test(userId)) {
      throw AuthError.unauthorized("Invalid user ID");
    }

    const roles = rolesStr ? rolesStr.split(",").filter(Boolean) : [];
    return { id: userId, roles };
  }
}

// ============================================================================
// Middleware factory
// ============================================================================

/**
 * Creates auth middleware using the provided validator.
 *
 * @example
 * ```typescript
 * // Development
 * const auth = createAuthMiddleware(new DevValidator());
 *
 * // Production
 * const auth = createAuthMiddleware(new HmacValidator(process.env.SECRET!, 3600));
 *
 * app.use("/api", auth.requireAuth, apiRouter);
 * ```
 */
export function createAuthMiddleware(validator: TokenValidator) {
  const extractToken = (req: Request): string | null => {
    const authHeader = req.headers.authorization;
    if (!authHeader) return null;

    const match = authHeader.match(/^Bearer\s+(.+)$/);
    return match ? match[1] : null;
  };

  return {
    /**
     * Middleware that requires authentication.
     */
    requireAuth: async (req: AuthenticatedRequest, res: Response, next: NextFunction): Promise<void> => {
      try {
        const token = extractToken(req);
        if (!token) {
          res.status(401).json({ error: "Missing Authorization header" });
          return;
        }

        req.user = await validator.validate(token);
        next();
      } catch (e) {
        if (e instanceof AuthError) {
          res.status(e.statusCode).json({ error: e.message });
        } else {
          res.status(401).json({ error: "Unauthorized" });
        }
      }
    },

    /**
     * Middleware that optionally extracts auth.
     */
    optionalAuth: async (req: AuthenticatedRequest, res: Response, next: NextFunction): Promise<void> => {
      try {
        const token = extractToken(req);
        if (token) {
          req.user = await validator.validate(token);
        }
      } catch {
        // Ignore validation errors for optional auth
      }
      next();
    },

    /**
     * Creates middleware that requires a specific role.
     */
    requireRole: (role: string) => {
      return async (req: AuthenticatedRequest, res: Response, next: NextFunction): Promise<void> => {
        try {
          const token = extractToken(req);
          if (!token) {
            res.status(401).json({ error: "Missing Authorization header" });
            return;
          }

          const user = await validator.validate(token);
          if (!user.roles.includes(role)) {
            res.status(403).json({ error: `Forbidden: requires role ${role}` });
            return;
          }

          req.user = user;
          next();
        } catch (e) {
          if (e instanceof AuthError) {
            res.status(e.statusCode).json({ error: e.message });
          } else {
            res.status(401).json({ error: "Unauthorized" });
          }
        }
      };
    },
  };
}

/**
 * Helper to check ownership in a handler.
 */
export function checkOwnership(
  user: AuthenticatedUser | undefined,
  resourceOwnerId: string
): void {
  if (!user) {
    throw AuthError.unauthorized();
  }
  if (user.id !== resourceOwnerId) {
    throw AuthError.forbidden("Not owner");
  }
}

// ============================================================================
// Legacy exports (deprecated, use createAuthMiddleware instead)
// ============================================================================

let _legacyValidator: TokenValidator | null = null;

/**
 * @deprecated Use createAuthMiddleware() instead
 */
export function requireAuth(req: AuthenticatedRequest, res: Response, next: NextFunction): void {
  if (!_legacyValidator) {
    _legacyValidator = new DevValidator();
  }
  createAuthMiddleware(_legacyValidator).requireAuth(req, res, next);
}

/**
 * @deprecated Use createAuthMiddleware() instead
 */
export function optionalAuth(req: AuthenticatedRequest, res: Response, next: NextFunction): void {
  if (!_legacyValidator) {
    _legacyValidator = new DevValidator();
  }
  createAuthMiddleware(_legacyValidator).optionalAuth(req, res, next);
}

/**
 * @deprecated Use createAuthMiddleware() instead
 */
export function requireRole(role: string) {
  if (!_legacyValidator) {
    _legacyValidator = new DevValidator();
  }
  return createAuthMiddleware(_legacyValidator).requireRole(role);
}
"#.to_string()
}

/// Generates TypeScript interfaces for all DTOs.
fn generate_types(dtos: &[Dto]) -> String {
    let mut code = String::from("// Generated types from Crudder schema\n\n");

    for dto in dtos {
        code.push_str(&generate_interface(dto));
        code.push('\n');
    }

    code
}

/// Generates a TypeScript interface for a DTO.
fn generate_interface(dto: &Dto) -> String {
    let mut code = format!("export interface {} {{\n", dto.name);

    for field in &dto.fields {
        let ts_type = type_ref_to_typescript(&field.ty);
        let optional = matches!(&field.ty, TypeRef::Optional(_));
        let field_name = to_camel_case(&field.name);

        if optional {
            code.push_str(&format!("  {}?: {};\n", field_name, ts_type));
        } else {
            code.push_str(&format!("  {}: {};\n", field_name, ts_type));
        }
    }

    code.push_str("}\n");
    code
}

/// Converts a TypeRef to a TypeScript type.
fn type_ref_to_typescript(ty: &TypeRef) -> String {
    match ty {
        TypeRef::Primitive(p) => primitive_to_typescript(p),
        TypeRef::Array(inner) => {
            let inner_ts = type_ref_to_typescript(inner);
            format!("{}[]", inner_ts)
        }
        TypeRef::Optional(inner) => {
            // The optional marker is handled at the field level
            type_ref_to_typescript(inner)
        }
        TypeRef::Named(name) => name.clone(),
    }
}

/// Converts a primitive type to TypeScript.
fn primitive_to_typescript(p: &PrimitiveType) -> String {
    match p {
        PrimitiveType::String => "string".to_string(),
        PrimitiveType::Int => "number".to_string(),
        PrimitiveType::Float => "number".to_string(),
        PrimitiveType::Bool => "boolean".to_string(),
        PrimitiveType::Uuid => "string".to_string(),
        PrimitiveType::Timestamp => "Date".to_string(),
        PrimitiveType::Bytes => "Uint8Array".to_string(),
    }
}

/// Generates Zod schemas for runtime validation.
fn generate_zod_schemas(dtos: &[Dto]) -> String {
    let mut code = String::from(
        r#"// Generated Zod schemas from Crudder schema

import { z } from "zod";

"#,
    );

    for dto in dtos {
        code.push_str(&generate_zod_schema(dto));
        code.push('\n');
    }

    code
}

/// Generates a Zod schema for a DTO.
fn generate_zod_schema(dto: &Dto) -> String {
    let schema_name = format!("{}Schema", dto.name);
    let mut code = format!("export const {} = z.object({{\n", schema_name);

    for field in &dto.fields {
        let zod_type = type_ref_to_zod(&field.ty);
        let field_name = to_camel_case(&field.name);
        code.push_str(&format!("  {}: {},\n", field_name, zod_type));
    }

    code.push_str("});\n");
    code.push_str(&format!(
        "export type {} = z.infer<typeof {}>;\n",
        dto.name, schema_name
    ));
    code
}

/// Converts a TypeRef to a Zod validator.
fn type_ref_to_zod(ty: &TypeRef) -> String {
    match ty {
        TypeRef::Primitive(p) => primitive_to_zod(p),
        TypeRef::Array(inner) => {
            let inner_zod = type_ref_to_zod(inner);
            format!("{}.array()", inner_zod)
        }
        TypeRef::Optional(inner) => {
            let inner_zod = type_ref_to_zod(inner);
            format!("{}.optional()", inner_zod)
        }
        TypeRef::Named(name) => format!("{}Schema", name),
    }
}

/// Converts a primitive type to Zod.
fn primitive_to_zod(p: &PrimitiveType) -> String {
    match p {
        PrimitiveType::String => "z.string()".to_string(),
        PrimitiveType::Int => "z.number().int()".to_string(),
        PrimitiveType::Float => "z.number()".to_string(),
        PrimitiveType::Bool => "z.boolean()".to_string(),
        PrimitiveType::Uuid => "z.string().uuid()".to_string(),
        PrimitiveType::Timestamp => "z.coerce.date()".to_string(),
        PrimitiveType::Bytes => "z.instanceof(Uint8Array)".to_string(),
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
                path_template = path.replace(&format!("{{{}}}", param_name), &format!("${{{}}}", param_name))
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
                path_template = path.replace(&format!("{{{}}}", param_name), &format!("${{{}}}", param_name))
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

/// Generates Express-style handlers.
fn generate_handlers(schema: &Schema) -> String {
    let mut code = String::from(
        r#"// Generated Express handlers from Crudder schema

import type { Request, Response, NextFunction, Router } from "express";
import type * as Types from "./types";
import type { AuthenticatedRequest, AuthenticatedUser } from "./auth";
import { requireAuth, optionalAuth, requireRole, checkOwnership } from "./auth";

export type Handler<TReq, TRes> = (
  req: TReq,
  res: Response,
  next: NextFunction
) => Promise<TRes | void>;

export type AuthHandler<TReq, TRes> = (
  req: TReq & { user: AuthenticatedUser },
  res: Response,
  next: NextFunction
) => Promise<TRes | void>;

"#,
    );

    for service in &schema.services {
        code.push_str(&format!("// {} handlers\n\n", service.name));

        for method in &service.methods {
            code.push_str(&generate_handler_type(method));
        }

        // Generate router setup function
        code.push_str(&generate_router_setup(service));
    }

    code
}

/// Generates a router setup function for a service.
fn generate_router_setup(service: &crudder_ast::Service) -> String {
    let _service_name = to_camel_case(&service.name);
    let mut code = format!(
        r#"/**
 * Creates an Express router for {}.
 * Pass in handler implementations for each endpoint.
 */
export function create{}Router(handlers: {{
"#,
        service.name, service.name
    );

    // Add handler type parameters
    for method in &service.methods {
        let fn_name = to_camel_case(&method.name);
        let handler_type = if method.is_public() {
            format!("{}Handler", fn_name)
        } else {
            format!("{}Handler", fn_name)
        };
        code.push_str(&format!("  {}: {};\n", fn_name, handler_type));
    }

    code.push_str("}): Router {\n");
    code.push_str("  const router = require('express').Router();\n\n");

    // Add routes with appropriate middleware
    for method in &service.methods {
        let fn_name = to_camel_case(&method.name);
        let http_method = get_http_method(method).to_lowercase();
        let path = get_path(method);

        let middleware = get_auth_middleware(method);

        if middleware.is_empty() {
            code.push_str(&format!(
                "  router.{}(\"{}\", handlers.{});\n",
                http_method, path, fn_name
            ));
        } else {
            code.push_str(&format!(
                "  router.{}(\"{}\", {}, handlers.{});\n",
                http_method, path, middleware, fn_name
            ));
        }
    }

    code.push_str("\n  return router;\n}\n\n");
    code
}

/// Gets the auth middleware to use for a method.
fn get_auth_middleware(method: &Method) -> String {
    match method.auth_requirement() {
        Some(AuthRequirement::Public) => String::new(),
        Some(AuthRequirement::Authenticated) => "requireAuth".to_string(),
        Some(AuthRequirement::Owner(_)) => "requireAuth".to_string(),
        Some(AuthRequirement::Role(role)) => format!("requireRole(\"{}\")", role),
        None => "requireAuth".to_string(), // Default to requiring auth
    }
}

/// Generates a handler type for a method.
fn generate_handler_type(method: &Method) -> String {
    let fn_name = to_camel_case(&method.name);
    let input_type = &method.input;
    let output_type = method
        .output
        .as_ref()
        .map(|o| format!("Types.{}", o))
        .unwrap_or_else(|| "void".to_string());

    let auth_req = method.auth_requirement();

    let (auth_comment, request_type, owner_note) = match &auth_req {
        Some(AuthRequirement::Public) => (
            "/** @public - No authentication required */".to_string(),
            "Request",
            String::new(),
        ),
        Some(AuthRequirement::Authenticated) => (
            "/** @authenticated - Requires valid auth token */".to_string(),
            "AuthenticatedRequest",
            String::new(),
        ),
        Some(AuthRequirement::Owner(field)) => (
            format!("/** @owner({}) - Requires auth + ownership check */", field),
            "AuthenticatedRequest",
            format!(
                "// NOTE: Call checkOwnership(req.user, resource.{}) after fetching the resource\n",
                to_camel_case(field)
            ),
        ),
        Some(AuthRequirement::Role(role)) => (
            format!("/** @role({}) - Requires auth + role */", role),
            "AuthenticatedRequest",
            String::new(),
        ),
        None => (
            "/** Requires authentication (default) */".to_string(),
            "AuthenticatedRequest",
            String::new(),
        ),
    };

    format!(
        r#"{auth_comment}
{owner_note}export type {fn_name}Handler = Handler<
  {request_type}<{{}}, {output_type}, Types.{input_type}>,
  {output_type}
>;

"#,
        auth_comment = auth_comment,
        owner_note = owner_note,
        fn_name = fn_name,
        request_type = request_type,
        input_type = input_type,
        output_type = output_type
    )
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

/// Converts a string to camelCase.
fn to_camel_case(s: &str) -> String {
    let snake = to_snake_case(s);
    let mut result = String::new();
    let mut capitalize_next = false;

    for c in snake.chars() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_uppercase().next().unwrap());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }

    result
}

/// Converts a string to snake_case.
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
    use crudder_ast::Field;

    #[test]
    fn test_to_camel_case() {
        assert_eq!(to_camel_case("CreateUser"), "createUser");
        assert_eq!(to_camel_case("get_user"), "getUser");
    }

    #[test]
    fn test_primitive_to_typescript() {
        assert_eq!(primitive_to_typescript(&PrimitiveType::String), "string");
        assert_eq!(primitive_to_typescript(&PrimitiveType::Int), "number");
        assert_eq!(primitive_to_typescript(&PrimitiveType::Bool), "boolean");
    }

    #[test]
    fn test_generate_interface() {
        let dto = Dto {
            name: "User".to_string(),
            fields: vec![
                Field {
                    name: "id".to_string(),
                    ty: TypeRef::Primitive(PrimitiveType::Uuid),
                    annotations: vec![],
                    span: None,
                },
                Field {
                    name: "name".to_string(),
                    ty: TypeRef::Primitive(PrimitiveType::String),
                    annotations: vec![],
                    span: None,
                },
            ],
            annotations: vec![],
            span: None,
        };

        let code = generate_interface(&dto);
        assert!(code.contains("export interface User"));
        assert!(code.contains("id: string"));
        assert!(code.contains("name: string"));
    }
}
