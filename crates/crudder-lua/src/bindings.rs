//! Lua bindings for Crudder AST types.

use crudder_ast::{
    Annotation, AuthRequirement, Dto, Field, Method, Schema, Service, StorageType, TypeRef,
};
use mlua::{FromLua, Lua, UserData, UserDataFields, UserDataMethods, Value};

/// Wrapper for Schema to expose to Lua.
#[derive(Clone)]
pub struct LuaSchema(pub Schema);

impl UserData for LuaSchema {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("dtos", |lua, this| {
            let dtos: Vec<LuaDto> = this.0.dtos.iter().cloned().map(LuaDto).collect();
            lua.create_sequence_from(dtos)
        });

        fields.add_field_method_get("services", |lua, this| {
            let services: Vec<LuaService> =
                this.0.services.iter().cloned().map(LuaService).collect();
            lua.create_sequence_from(services)
        });
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("get_dto", |_, this, name: String| {
            Ok(this.0.get_dto(&name).cloned().map(LuaDto))
        });

        methods.add_method("get_service", |_, this, name: String| {
            Ok(this.0.get_service(&name).cloned().map(LuaService))
        });

        methods.add_method("entities", |lua, this, ()| {
            let entities: Vec<LuaDto> = this.0.entities().cloned().map(LuaDto).collect();
            lua.create_sequence_from(entities)
        });
    }
}

/// Wrapper for Dto to expose to Lua.
#[derive(Clone)]
pub struct LuaDto(pub Dto);

impl UserData for LuaDto {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("name", |_, this| Ok(this.0.name.clone()));

        fields.add_field_method_get("fields", |lua, this| {
            let fields: Vec<LuaField> = this.0.fields.iter().cloned().map(LuaField).collect();
            lua.create_sequence_from(fields)
        });

        fields.add_field_method_get("annotations", |lua, this| {
            let anns: Vec<LuaAnnotation> =
                this.0.annotations.iter().cloned().map(LuaAnnotation).collect();
            lua.create_sequence_from(anns)
        });
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_entity", |_, this, ()| Ok(this.0.is_entity()));

        methods.add_method("table_name", |_, this, ()| {
            Ok(this.0.table_name().map(|s| s.to_string()))
        });

        methods.add_method("primary_key", |_, this, ()| {
            Ok(this.0.primary_key().cloned().map(LuaField))
        });
    }
}

/// Wrapper for Field to expose to Lua.
#[derive(Clone)]
pub struct LuaField(pub Field);

impl UserData for LuaField {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("name", |_, this| Ok(this.0.name.clone()));

        fields.add_field_method_get("ty", |_, this| Ok(LuaTypeRef(this.0.ty.clone())));

        fields.add_field_method_get("annotations", |lua, this| {
            let anns: Vec<LuaAnnotation> =
                this.0.annotations.iter().cloned().map(LuaAnnotation).collect();
            lua.create_sequence_from(anns)
        });

        fields.add_field_method_get("index", |_, this| Ok(this.0.index));
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_primary", |_, this, ()| Ok(this.0.is_primary()));

        methods.add_method("is_auto", |_, this, ()| Ok(this.0.is_auto()));

        methods.add_method("column_name", |_, this, ()| {
            Ok(this.0.column_name().to_string())
        });

        methods.add_method("references", |_, this, ()| {
            Ok(this.0.references().map(|s| s.to_string()))
        });
    }
}

/// Wrapper for TypeRef to expose to Lua.
#[derive(Clone)]
pub struct LuaTypeRef(pub TypeRef);

impl FromLua for LuaTypeRef {
    fn from_lua(value: Value, _lua: &Lua) -> mlua::Result<Self> {
        match value {
            Value::UserData(ud) => ud.borrow::<LuaTypeRef>().map(|r| r.clone()),
            _ => Err(mlua::Error::FromLuaConversionError {
                from: value.type_name(),
                to: "LuaTypeRef".to_string(),
                message: Some("expected a TypeRef userdata".to_string()),
            }),
        }
    }
}

impl UserData for LuaTypeRef {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        // kind: "primitive", "array", "optional", "named"
        fields.add_field_method_get("kind", |_, this| {
            Ok(match &this.0 {
                TypeRef::Primitive(_) => "primitive",
                TypeRef::Array(_) => "array",
                TypeRef::Optional(_) => "optional",
                TypeRef::Named(_) => "named",
            })
        });

        // primitive: string name if this is a primitive type
        fields.add_field_method_get("primitive", |_, this| {
            Ok(match &this.0 {
                TypeRef::Primitive(p) => Some(p.as_str().to_string()),
                _ => None,
            })
        });

        // inner: the inner type for array/optional
        fields.add_field_method_get("inner", |_, this| {
            Ok(match &this.0 {
                TypeRef::Array(inner) => Some(LuaTypeRef(*inner.clone())),
                TypeRef::Optional(inner) => Some(LuaTypeRef(*inner.clone())),
                _ => None,
            })
        });

        // name: the name for named types
        fields.add_field_method_get("name", |_, this| {
            Ok(match &this.0 {
                TypeRef::Named(name) => Some(name.clone()),
                _ => None,
            })
        });
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        // Helper to get a string representation
        methods.add_method("to_string", |_, this, ()| Ok(type_ref_to_string(&this.0)));

        // is_optional helper
        methods.add_method("is_optional", |_, this, ()| {
            Ok(matches!(&this.0, TypeRef::Optional(_)))
        });

        // is_array helper
        methods.add_method("is_array", |_, this, ()| {
            Ok(matches!(&this.0, TypeRef::Array(_)))
        });

        // Unwrap to get the base type (unwraps optional/array)
        methods.add_method("base_type", |_, this, ()| {
            fn unwrap_type(ty: &TypeRef) -> &TypeRef {
                match ty {
                    TypeRef::Array(inner) => unwrap_type(inner),
                    TypeRef::Optional(inner) => unwrap_type(inner),
                    _ => ty,
                }
            }
            Ok(LuaTypeRef(unwrap_type(&this.0).clone()))
        });
    }
}

fn type_ref_to_string(ty: &TypeRef) -> String {
    match ty {
        TypeRef::Primitive(p) => p.as_str().to_string(),
        TypeRef::Array(inner) => format!("[]{}", type_ref_to_string(inner)),
        TypeRef::Optional(inner) => format!("{}?", type_ref_to_string(inner)),
        TypeRef::Named(name) => name.clone(),
    }
}

/// Wrapper for Annotation to expose to Lua.
#[derive(Clone)]
pub struct LuaAnnotation(pub Annotation);

impl UserData for LuaAnnotation {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("name", |_, this| Ok(this.0.name.clone()));

        fields.add_field_method_get("args", |lua, this| {
            lua.create_sequence_from(this.0.args.iter().cloned())
        });
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_primary", |_, this, ()| Ok(this.0.is_primary()));
        methods.add_method("is_auto", |_, this, ()| Ok(this.0.is_auto()));
        methods.add_method("table_name", |_, this, ()| {
            Ok(this.0.table_name().map(|s| s.to_string()))
        });
        methods.add_method("column_name", |_, this, ()| {
            Ok(this.0.column_name().map(|s| s.to_string()))
        });
        methods.add_method("references", |_, this, ()| {
            Ok(this.0.references().map(|s| s.to_string()))
        });
    }
}

/// Wrapper for Service to expose to Lua.
#[derive(Clone)]
pub struct LuaService(pub Service);

impl UserData for LuaService {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("name", |_, this| Ok(this.0.name.clone()));

        fields.add_field_method_get("methods", |lua, this| {
            let methods: Vec<LuaMethod> = this.0.methods.iter().cloned().map(LuaMethod).collect();
            lua.create_sequence_from(methods)
        });

        fields.add_field_method_get("annotations", |lua, this| {
            let anns: Vec<LuaAnnotation> =
                this.0.annotations.iter().cloned().map(LuaAnnotation).collect();
            lua.create_sequence_from(anns)
        });
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("storage_type", |_, this, ()| {
            Ok(this.0.storage_type().map(|st| match st {
                StorageType::Memory => "memory",
                StorageType::Postgres => "postgres",
                StorageType::Sqlite => "sqlite",
            }))
        });
    }
}

/// Wrapper for Method to expose to Lua.
#[derive(Clone)]
pub struct LuaMethod(pub Method);

impl UserData for LuaMethod {
    fn add_fields<F: UserDataFields<Self>>(fields: &mut F) {
        fields.add_field_method_get("name", |_, this| Ok(this.0.name.clone()));
        fields.add_field_method_get("input", |_, this| Ok(this.0.input.clone()));
        fields.add_field_method_get("output", |_, this| Ok(this.0.output.clone()));

        fields.add_field_method_get("annotations", |lua, this| {
            let anns: Vec<LuaAnnotation> =
                this.0.annotations.iter().cloned().map(LuaAnnotation).collect();
            lua.create_sequence_from(anns)
        });
    }

    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("is_public", |_, this, ()| Ok(this.0.is_public()));
        methods.add_method("requires_auth", |_, this, ()| Ok(this.0.requires_auth()));

        methods.add_method("auth_requirement", |_, this, ()| {
            Ok(this.0.auth_requirement().map(|ar| match ar {
                AuthRequirement::Public => "public".to_string(),
                AuthRequirement::Authenticated => "authenticated".to_string(),
                AuthRequirement::Owner(field) => format!("owner:{}", field),
                AuthRequirement::Role(role) => format!("role:{}", role),
            }))
        });

        // Get HTTP method from @rest annotation
        methods.add_method("http_method", |_, this, ()| {
            for ann in &this.0.annotations {
                if ann.name == "rest" {
                    if let Some(method) = ann.args.first() {
                        return Ok(Some(method.clone()));
                    }
                }
            }
            Ok(None)
        });

        // Get HTTP path from @rest annotation
        methods.add_method("http_path", |_, this, ()| {
            for ann in &this.0.annotations {
                if ann.name == "rest" {
                    if let Some(path) = ann.args.get(1) {
                        return Ok(Some(path.clone()));
                    }
                }
            }
            Ok(None)
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crudder_ast::PrimitiveType;
    use mlua::Lua;

    fn create_test_schema() -> Schema {
        Schema {
            dtos: vec![
                Dto {
                    name: "Todo".to_string(),
                    fields: vec![
                        Field {
                            name: "id".to_string(),
                            ty: TypeRef::Primitive(PrimitiveType::Uuid),
                            annotations: vec![
                                Annotation::new("primary"),
                                Annotation::new("auto"),
                            ],
                            index: None,
                            span: None,
                        },
                        Field {
                            name: "title".to_string(),
                            ty: TypeRef::Primitive(PrimitiveType::String),
                            annotations: vec![],
                            index: None,
                            span: None,
                        },
                        Field {
                            name: "tags".to_string(),
                            ty: TypeRef::Array(Box::new(TypeRef::Primitive(PrimitiveType::String))),
                            annotations: vec![],
                            index: None,
                            span: None,
                        },
                    ],
                    annotations: vec![Annotation::with_args("table", vec!["todos".to_string()])],
                    span: None,
                },
                Dto {
                    name: "CreateTodoRequest".to_string(),
                    fields: vec![Field {
                        name: "title".to_string(),
                        ty: TypeRef::Primitive(PrimitiveType::String),
                        annotations: vec![],
                        index: None,
                        span: None,
                    }],
                    annotations: vec![],
                    span: None,
                },
            ],
            services: vec![Service {
                name: "TodoService".to_string(),
                methods: vec![
                    Method {
                        name: "ListTodos".to_string(),
                        input: "Empty".to_string(),
                        output: Some("TodoList".to_string()),
                        annotations: vec![
                            Annotation::new("public"),
                            Annotation::with_args(
                                "rest",
                                vec!["GET".to_string(), "/todos".to_string()],
                            ),
                        ],
                        span: None,
                    },
                    Method {
                        name: "CreateTodo".to_string(),
                        input: "CreateTodoRequest".to_string(),
                        output: Some("Todo".to_string()),
                        annotations: vec![
                            Annotation::new("authenticated"),
                            Annotation::with_args(
                                "rest",
                                vec!["POST".to_string(), "/todos".to_string()],
                            ),
                        ],
                        span: None,
                    },
                ],
                annotations: vec![Annotation::with_args(
                    "storage",
                    vec!["postgres".to_string()],
                )],
                span: None,
            }],
        }
    }

    #[test]
    fn test_schema_bindings() {
        let lua = Lua::new();
        let schema = LuaSchema(create_test_schema());

        lua.scope(|scope| {
            let schema_ud = scope.create_userdata(schema).unwrap();
            lua.globals().set("schema", schema_ud).unwrap();

            // Test dtos
            let count: i32 = lua.load("#schema.dtos").eval().unwrap();
            assert_eq!(count, 2);

            // Test get_dto
            let name: String = lua.load("schema:get_dto('Todo').name").eval().unwrap();
            assert_eq!(name, "Todo");

            // Test entities
            let entities_count: i32 = lua.load("#schema:entities()").eval().unwrap();
            assert_eq!(entities_count, 1);

            Ok(())
        })
        .unwrap();
    }

    #[test]
    fn test_dto_bindings() {
        let lua = Lua::new();
        let schema = LuaSchema(create_test_schema());

        lua.scope(|scope| {
            let schema_ud = scope.create_userdata(schema).unwrap();
            lua.globals().set("schema", schema_ud).unwrap();

            // Test DTO properties
            lua.load(
                r#"
                local todo = schema:get_dto('Todo')
                assert(todo.name == 'Todo')
                assert(todo:is_entity() == true)
                assert(todo:table_name() == 'todos')
                assert(#todo.fields == 3)
            "#,
            )
            .exec()
            .unwrap();

            // Test non-entity DTO
            lua.load(
                r#"
                local req = schema:get_dto('CreateTodoRequest')
                assert(req:is_entity() == false)
                assert(req:table_name() == nil)
            "#,
            )
            .exec()
            .unwrap();

            Ok(())
        })
        .unwrap();
    }

    #[test]
    fn test_field_bindings() {
        let lua = Lua::new();
        let schema = LuaSchema(create_test_schema());

        lua.scope(|scope| {
            let schema_ud = scope.create_userdata(schema).unwrap();
            lua.globals().set("schema", schema_ud).unwrap();

            lua.load(
                r#"
                local todo = schema:get_dto('Todo')
                local id_field = todo.fields[1]
                assert(id_field.name == 'id')
                assert(id_field:is_primary() == true)
                assert(id_field:is_auto() == true)
                assert(id_field.ty.kind == 'primitive')
                assert(id_field.ty.primitive == 'uuid')
            "#,
            )
            .exec()
            .unwrap();

            Ok(())
        })
        .unwrap();
    }

    #[test]
    fn test_type_ref_bindings() {
        let lua = Lua::new();
        let schema = LuaSchema(create_test_schema());

        lua.scope(|scope| {
            let schema_ud = scope.create_userdata(schema).unwrap();
            lua.globals().set("schema", schema_ud).unwrap();

            lua.load(
                r#"
                local todo = schema:get_dto('Todo')
                local tags_field = todo.fields[3]
                assert(tags_field.ty.kind == 'array')
                assert(tags_field.ty.inner.kind == 'primitive')
                assert(tags_field.ty.inner.primitive == 'string')
                assert(tags_field.ty:is_array() == true)
            "#,
            )
            .exec()
            .unwrap();

            Ok(())
        })
        .unwrap();
    }

    #[test]
    fn test_service_bindings() {
        let lua = Lua::new();
        let schema = LuaSchema(create_test_schema());

        lua.scope(|scope| {
            let schema_ud = scope.create_userdata(schema).unwrap();
            lua.globals().set("schema", schema_ud).unwrap();

            lua.load(
                r#"
                local service = schema.services[1]
                assert(service.name == 'TodoService')
                assert(service:storage_type() == 'postgres')
                assert(#service.methods == 2)
            "#,
            )
            .exec()
            .unwrap();

            Ok(())
        })
        .unwrap();
    }

    #[test]
    fn test_method_bindings() {
        let lua = Lua::new();
        let schema = LuaSchema(create_test_schema());

        lua.scope(|scope| {
            let schema_ud = scope.create_userdata(schema).unwrap();
            lua.globals().set("schema", schema_ud).unwrap();

            lua.load(
                r#"
                local service = schema.services[1]
                local list_method = service.methods[1]
                assert(list_method.name == 'ListTodos')
                assert(list_method.input == 'Empty')
                assert(list_method.output == 'TodoList')
                assert(list_method:is_public() == true)
                assert(list_method:http_method() == 'GET')
                assert(list_method:http_path() == '/todos')

                local create_method = service.methods[2]
                assert(create_method:is_public() == false)
                assert(create_method:requires_auth() == true)
                assert(create_method:http_method() == 'POST')
            "#,
            )
            .exec()
            .unwrap();

            Ok(())
        })
        .unwrap();
    }
}
