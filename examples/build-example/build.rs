fn main() {
    crudder_build::Config::new()
        .sqlx("postgres")
        .compile(&["../../examples/todo.crudder"])
        .unwrap();
}
