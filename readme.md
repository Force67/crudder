# crudder

A transport-agnostic DSL for defining API contracts with code generation for REST (Rust/TypeScript) and Protobuf.

## overview

Crudder lets you define your API schema once and generate:
- **Rust** - Axum handlers with SQLx database integration
- **TypeScript** - Types, Zod schemas, Express handlers, and API client
- **Protobuf** - `.proto` files for gRPC

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────────┐
│  schema.crudder │ ──▶ │   crudder    │ ──▶ │  Generated Code │
└─────────────────┘     │   codegen    │     ├─────────────────┤
                        └──────────────┘     │ • Rust/Axum     │
                                             │ • TypeScript    │
                                             │ • Protobuf      │
                                             └─────────────────┘
```

## installation

```bash
cargo install --path crates/crudder-cli
```

## usage

```bash
# generate rust code with sqlx
crudder generate schema.crudder --target sqlx-postgres --output ./generated

# generate typescript
crudder generate schema.crudder --target typescript --output ./generated

# generate protobuf
crudder generate schema.crudder --target protobuf --output ./generated

# validate schema without generating
crudder check schema.crudder
```

## schema syntax

### data transfer objects (DTOs)

DTOs define the shape of data. Use `@table` to mark a DTO as a database entity.

```
@table("users")
dto User {
    @primary @auto id: uuid
    name: string
    email: string
    @auto createdAt: timestamp
}

dto CreateUserRequest {
    name: string
    email: string
}

dto UserList {
    users: []User
}
```

### primitive types

| type | rust | typescript | sql |
|------|------|------------|-----|
| `string` | `String` | `string` | `TEXT` |
| `int` | `i64` | `number` | `BIGINT` |
| `float` | `f64` | `number` | `DOUBLE PRECISION` |
| `bool` | `bool` | `boolean` | `BOOLEAN` |
| `uuid` | `Uuid` | `string` | `UUID` |
| `timestamp` | `DateTime<Utc>` | `Date` | `TIMESTAMPTZ` |
| `bytes` | `Vec<u8>` | `Uint8Array` | `BYTEA` |

### type modifiers

```
field: string       // required
field: string?      // optional (nullable)
field: []string     // array
field: []User?      // optional array of User
```

### field annotations

| annotation | description |
|------------|-------------|
| `@primary` | marks field as primary key |
| `@auto` | auto-generated (uuid default, timestamp default NOW()) |
| `@column("name")` | custom SQL column name |
| `@references(Dto)` | foreign key reference |

### services

Services define API endpoints.

```
@storage("postgres")
service UserService {
    @public
    @rest("GET", "/users")
    ListUsers(Empty) -> UserList

    @authenticated
    @rest("POST", "/users")
    CreateUser(CreateUserRequest) -> User

    @owner("userId")
    @rest("PUT", "/users/{id}")
    UpdateUser(UpdateUserRequest) -> User

    @role("admin")
    @rest("DELETE", "/users/{id}")
    DeleteUser(Empty) -> Empty
}
```

### service annotations

| annotation | description |
|------------|-------------|
| `@storage("postgres")` | database backend (postgres, sqlite, memory) |
| `@rest("METHOD", "/path")` | HTTP method and path |

### auth annotations

| annotation | description |
|------------|-------------|
| `@public` | no authentication required |
| `@authenticated` | requires valid auth token |
| `@owner("field")` | requires auth + resource ownership check |
| `@role("name")` | requires auth + specific role |

if no auth annotation is specified, `@authenticated` is assumed (secure by default).

## authentication

generated code includes a pluggable authentication system.

### rust

```rust
use myapp::auth::{TokenValidator, DevValidator, HmacValidator};
use myapp::todo_service::AppState;

// development (INSECURE - prints warning)
let state = AppState::new(pool, DevValidator::new());

// production - HMAC tokens
let state = AppState::new(pool, HmacValidator::new(b"secret-key-32-chars-min", 3600));

// production - custom validator (JWT, sessions, etc.)
struct JwtValidator { /* ... */ }
impl TokenValidator for JwtValidator {
    fn validate(&self, token: &str) -> Result<AuthenticatedUser, AuthError> {
        // validate JWT and extract claims
    }
}
let state = AppState::new(pool, JwtValidator::new());
```

### typescript

```typescript
import { createAuthMiddleware, DevValidator, HmacValidator } from "./auth";

// development (INSECURE - prints warning)
const auth = createAuthMiddleware(new DevValidator());

// production - HMAC tokens
const auth = createAuthMiddleware(new HmacValidator(process.env.SECRET!, 3600));

// use middleware
app.use("/api", auth.requireAuth, apiRouter);
```

### token format

the built-in `HmacValidator` uses this token format:

```
{uuid}:{roles}:{timestamp}:{signature}

example:
550e8400-e29b-41d4-a716-446655440000:user,admin:1706745600:a1b2c3d4...
```

- `uuid` - user identifier
- `roles` - comma-separated role list
- `timestamp` - unix timestamp (seconds)
- `signature` - HMAC-SHA256 hex signature

generate tokens:

```rust
let token = validator.generate_token(user_id, &["user", "admin"]);
```

```typescript
const token = validator.generateToken(userId, ["user", "admin"]);
```

### security properties

| property | DevValidator | HmacValidator |
|----------|--------------|---------------|
| forgery protection | ✗ | ✓ (HMAC-SHA256) |
| expiration | ✗ | ✓ (configurable TTL) |
| timing attack resistant | n/a | ✓ (constant-time compare) |

⚠️ `DevValidator` is for development only. it trusts client-provided tokens and prints a warning on startup.

## complete example

```
// todo.crudder

@table("todos")
dto Todo {
    @primary @auto id: uuid
    title: string
    completed: bool
    @references(User) userId: uuid
    @auto createdAt: timestamp
}

dto CreateTodoRequest {
    title: string
}

dto UpdateTodoRequest {
    title: string?
    completed: bool?
}

dto TodoList {
    todos: []Todo
}

dto Empty {}

@storage("postgres")
service TodoService {
    @public
    @rest("GET", "/todos")
    ListTodos(Empty) -> TodoList

    @authenticated
    @rest("POST", "/todos")
    CreateTodo(CreateTodoRequest) -> Todo

    @authenticated
    @rest("GET", "/todos/{id}")
    GetTodo(Empty) -> Todo

    @owner("userId")
    @rest("PUT", "/todos/{id}")
    UpdateTodo(UpdateTodoRequest) -> Todo

    @owner("userId")
    @rest("DELETE", "/todos/{id}")
    DeleteTodo(Empty) -> Empty
}
```

generate and use:

```bash
crudder generate todo.crudder --target sqlx-postgres --output ./src/generated
```

```rust
use generated::{auth::HmacValidator, todo_service::AppState, router};

#[tokio::main]
async fn main() {
    let pool = PgPool::connect(&env::var("DATABASE_URL").unwrap()).await.unwrap();
    let validator = HmacValidator::new(env::var("AUTH_SECRET").unwrap().as_bytes(), 3600);
    let state = AppState::new(pool, validator);

    let app = router().with_state(state);
    axum::serve(listener, app).await.unwrap();
}
```

## project structure

```
crudder/
├── crates/
│   ├── crudder-ast/      # core AST types
│   ├── crudder-lexer/    # tokenizer (logos)
│   ├── crudder-parser/   # parser with error reporting (ariadne)
│   ├── crudder-codegen/  # code generators
│   └── crudder-cli/      # CLI tool
├── examples/             # example .crudder files
└── tests/security/       # security tests
```

## license

MIT
