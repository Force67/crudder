# crudder

I often find myself writing backends in other languages than the frontend, however i want the api contracts in sync. So inspired by protobuf i quickly made a DSL. It can emit rest and protobuf handlers, and that in rust and ts atm.

The codegen is inspired by premake5, so you can declare it in pure lua, with bindings for the nessesary rust stuff, and easily provide custom generations for your stuff.
A few "recommended" recipes are provided, such as for example rust-axum-serde for rust, for that check the recipes folder.

## Q&A
### Why not generate the types from an openapi spec?
Because i want to have more control over the generated code, and i want database support too, so the dto's can also become database tables.


## Scheme
Read the spec.md file.
