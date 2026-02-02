//! Crudder CLI tool.

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use crudder_lua::{RecipeLoader, RecipeRunner};
use crudder_parser::parse;

#[derive(Parser)]
#[command(name = "crudder")]
#[command(author, version, about = "A transport-agnostic DSL for defining API contracts", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate code from a .crudder file using a recipe
    Generate {
        /// Input .crudder file
        #[arg(short, long)]
        input: PathBuf,

        /// Recipe to use (e.g., rust-axum, typescript-react)
        #[arg(short, long)]
        recipe: String,

        /// Output directory
        #[arg(short, long)]
        output: PathBuf,

        /// Set recipe options (key=value)
        #[arg(long = "set", value_parser = parse_key_value)]
        options: Vec<(String, String)>,
    },

    /// Validate a .crudder file without generating code
    Check {
        /// Input .crudder file
        #[arg(short, long)]
        input: PathBuf,
    },

    /// Format a .crudder file
    Fmt {
        /// Input .crudder file
        input: PathBuf,

        /// Write output back to file (default: print to stdout)
        #[arg(short, long)]
        write: bool,
    },

    /// List available recipes
    Recipes,
}

fn parse_key_value(s: &str) -> Result<(String, String), String> {
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid option format: '{}', expected key=value", s))?;
    Ok((s[..pos].to_string(), s[pos + 1..].to_string()))
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Generate {
            input,
            recipe,
            output,
            options,
        } => run_generate(&input, &recipe, &output, options),
        Commands::Check { input } => run_check(&input),
        Commands::Fmt { input, write } => run_fmt(&input, write),
        Commands::Recipes => run_list_recipes(),
    };

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

fn run_generate(
    input: &PathBuf,
    recipe_name: &str,
    output: &PathBuf,
    options: Vec<(String, String)>,
) -> Result<(), Box<dyn std::error::Error>> {
    let source = std::fs::read_to_string(input)?;
    let filename = input.display().to_string();

    let schema = match parse(&source) {
        Ok(schema) => schema,
        Err(e) => {
            e.report(&filename, &source);
            return Err("parsing failed".into());
        }
    };

    let loader = RecipeLoader::new().with_project_dir(
        input
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| PathBuf::from(".")),
    );
    let recipe = loader.load(recipe_name)?;

    let mut runner = RecipeRunner::new();
    for (key, value) in options {
        runner.set_option(key, value);
    }

    let files = runner.run(&schema, &recipe)?;

    std::fs::create_dir_all(output)?;

    for file in &files {
        let path = output.join(&file.path);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&path, &file.content)?;
    }

    println!(
        "Generated {} files to {}",
        files.len(),
        output.display()
    );
    for file in &files {
        println!("  - {}", file.path);
    }

    Ok(())
}

fn run_check(input: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let source = std::fs::read_to_string(input)?;
    let filename = input.display().to_string();

    match parse(&source) {
        Ok(schema) => {
            println!("✓ {} is valid", filename);
            println!("  {} DTOs", schema.dtos.len());
            println!("  {} services", schema.services.len());

            for dto in &schema.dtos {
                println!("    dto {} ({} fields)", dto.name, dto.fields.len());
            }

            for service in &schema.services {
                println!(
                    "    service {} ({} methods)",
                    service.name,
                    service.methods.len()
                );
            }

            Ok(())
        }
        Err(e) => {
            e.report(&filename, &source);
            Err("validation failed".into())
        }
    }
}

fn run_fmt(input: &PathBuf, write: bool) -> Result<(), Box<dyn std::error::Error>> {
    let source = std::fs::read_to_string(input)?;
    let filename = input.display().to_string();

    let schema = match parse(&source) {
        Ok(schema) => schema,
        Err(e) => {
            e.report(&filename, &source);
            return Err("parsing failed".into());
        }
    };

    let formatted = format_schema(&schema);

    if write {
        std::fs::write(input, &formatted)?;
        println!("Formatted {}", filename);
    } else {
        print!("{}", formatted);
    }

    Ok(())
}

fn run_list_recipes() -> Result<(), Box<dyn std::error::Error>> {
    let loader = RecipeLoader::new();
    let recipes = loader.list_recipes();

    println!("Available recipes:");
    println!();

    for recipe in recipes {
        let source = if recipe.builtin {
            "(built-in)"
        } else {
            "(user)"
        };
        println!("  {} - {} {}", recipe.name, recipe.description, source);
    }

    Ok(())
}

/// Formats a schema back to source code.
fn format_schema(schema: &crudder_ast::Schema) -> String {
    let mut output = String::new();

    for (i, dto) in schema.dtos.iter().enumerate() {
        if i > 0 {
            output.push('\n');
        }
        output.push_str(&format_dto(dto));
    }

    for service in &schema.services {
        output.push('\n');
        output.push_str(&format_service(service));
    }

    output
}

fn format_dto(dto: &crudder_ast::Dto) -> String {
    let mut output = String::new();

    // Format annotations
    for ann in &dto.annotations {
        output.push_str(&format_annotation(ann));
        output.push('\n');
    }

    output.push_str(&format!("dto {} {{\n", dto.name));

    for field in &dto.fields {
        // Format field annotations
        for ann in &field.annotations {
            output.push_str("    ");
            output.push_str(&format_annotation(ann));
            output.push(' ');
        }
        if !field.annotations.is_empty() {
            // Remove trailing space, add field on same line
            output.pop();
            output.push_str(&format!(" {}: {}\n", field.name, format_type(&field.ty)));
        } else {
            output.push_str(&format!("    {}: {}\n", field.name, format_type(&field.ty)));
        }
    }

    output.push_str("}\n");
    output
}

fn format_annotation(ann: &crudder_ast::Annotation) -> String {
    if ann.args.is_empty() {
        format!("@{}", ann.name)
    } else {
        format!(
            "@{}({})",
            ann.name,
            ann.args
                .iter()
                .map(|a| format!("\"{}\"", a))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

fn format_type(ty: &crudder_ast::TypeRef) -> String {
    match ty {
        crudder_ast::TypeRef::Primitive(p) => p.as_str().to_string(),
        crudder_ast::TypeRef::Array(inner) => format!("[]{}", format_type(inner)),
        crudder_ast::TypeRef::Optional(inner) => format!("{}?", format_type(inner)),
        crudder_ast::TypeRef::Named(name) => name.clone(),
    }
}

fn format_service(service: &crudder_ast::Service) -> String {
    let mut output = String::new();

    // Format service annotations
    for ann in &service.annotations {
        output.push_str(&format_annotation(ann));
        output.push('\n');
    }

    output.push_str(&format!("service {} {{\n", service.name));

    for method in &service.methods {
        // Format method annotations
        for ann in &method.annotations {
            output.push_str("    ");
            output.push_str(&format_annotation(ann));
            output.push('\n');
        }

        let return_type = method.output.as_ref().map(|s| s.as_str()).unwrap_or("void");

        output.push_str(&format!(
            "    {}({}) -> {}\n",
            method.name, method.input, return_type
        ));

        // Add blank line between methods
        output.push('\n');
    }

    // Remove trailing blank line
    if output.ends_with("\n\n") {
        output.pop();
    }

    output.push_str("}\n");
    output
}
