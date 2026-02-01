# crudder

I often find myself writing backends in other languages than the frontend, however i want the api contracts in sync. So inspired by protobuf i quickly made a DSL. It can emit rest and protobuf handlers, and that in rust and ts atm.

## Q&A
### Why not generate the types from an openapi spec?
Because i want to have more control over the generated code, and i want database support too, so the dto's can also become database tables.


## Scheme
Read the spec.md file.
