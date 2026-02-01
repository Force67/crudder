//! Express handlers pass for TypeScript.

use crudder_ast::{AuthRequirement, Method, Schema, Service};

use crate::pass::{GenerationContext, Pass};
use crate::typescript::base::{to_camel_case, to_snake_case};
use crate::CodegenError;

/// Express pass that generates Express-style handlers and routing.
///
/// This pass creates:
/// - `handlers.ts` - Handler type definitions and router setup
/// - `auth.ts` - Authentication module with pluggable validators
///
/// It depends on the `typescript-base` pass and optionally uses Zod if available.
pub struct ExpressPass;

impl Pass for ExpressPass {
    fn name(&self) -> &'static str {
        "express"
    }

    fn depends_on(&self) -> &[&'static str] {
        &["typescript-base"]
    }

    fn run(&self, schema: &Schema, ctx: &mut GenerationContext) -> Result<(), CodegenError> {
        let has_zod = ctx.has_metadata("has:zod");

        // Generate handlers.ts
        let handlers = generate_express_handlers(schema, has_zod);
        ctx.set_file("handlers.ts", handlers);

        // Generate auth.ts
        let auth = generate_express_auth();
        ctx.set_file("auth.ts", auth);

        // Mark that Express is available
        ctx.set_metadata("has:express", "true");

        Ok(())
    }
}

/// Generates Express-style handlers.
fn generate_express_handlers(schema: &Schema, has_zod: bool) -> String {
    let mut code = String::from(
        r#"// Generated Express handlers from Crudder schema

import type { Request, Response, NextFunction, Router } from "express";
import type * as Types from "./types";
import type { AuthenticatedRequest, AuthenticatedUser } from "./auth";
import { requireAuth, optionalAuth, requireRole, checkOwnership } from "./auth";
"#,
    );

    if has_zod {
        code.push_str("import * as Schemas from \"./schemas\";\n");
    }

    code.push_str(
        r#"
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
            code.push_str(&generate_handler_type(method, has_zod));
        }

        // Generate router setup function
        code.push_str(&generate_router_setup(service));
    }

    code
}

/// Generates a handler type for a method.
fn generate_handler_type(method: &Method, has_zod: bool) -> String {
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
            format!(
                "/** @owner({}) - Requires auth + ownership check */",
                field
            ),
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

    let validation_note = if has_zod {
        format!(
            "// Validate request with Schemas.{}Schema.parse(req.body)\n",
            input_type
        )
    } else {
        String::new()
    };

    format!(
        r#"{auth_comment}
{owner_note}{validation_note}export type {fn_name}Handler = Handler<
  {request_type}<{{}}, {output_type}, Types.{input_type}>,
  {output_type}
>;

"#,
        auth_comment = auth_comment,
        owner_note = owner_note,
        validation_note = validation_note,
        fn_name = fn_name,
        request_type = request_type,
        input_type = input_type,
        output_type = output_type
    )
}

/// Generates a router setup function for a service.
fn generate_router_setup(service: &Service) -> String {
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
        let handler_type = format!("{}Handler", fn_name);
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

/// Generates the auth module with pluggable token validation.
fn generate_express_auth() -> String {
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
