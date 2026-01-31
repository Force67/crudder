//! Crudder CLI tool.

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use crudder_codegen::{generator_for, Target};
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
    /// Generate code from a .crudder file
    Generate {
        /// Input .crudder file
        input: PathBuf,

        /// Target language (rust, typescript, protobuf)
        #[arg(short, long)]
        target: String,

        /// Output directory
        #[arg(short, long)]
        output: PathBuf,
    },

    /// Validate a .crudder file without generating code
    Check {
        /// Input .crudder file
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
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Generate {
            input,
            target,
            output,
        } => run_generate(&input, &target, &output),
        Commands::Check { input } => run_check(&input),
        Commands::Fmt { input, write } => run_fmt(&input, write),
    };

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

fn run_generate(input: &PathBuf, target: &str, output: &PathBuf) -> Result<(), Box<dyn std::error::Error>> {
    let source = std::fs::read_to_string(input)?;
    let filename = input.display().to_string();

    let schema = match parse(&source) {
        Ok(schema) => schema,
        Err(e) => {
            e.report(&filename, &source);
            return Err("parsing failed".into());
        }
    };

    let target: Target = target.parse().map_err(|e: String| e)?;
    let generator = generator_for(target);

    let files = generator.generate(&schema)?;

    std::fs::create_dir_all(output)?;
    files.write_to(output)?;

    println!(
        "Generated {} files to {}",
        files.files.len(),
        output.display()
    );
    for file in &files.files {
        println!("  - {}", file.path.display());
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
    let mut output = format!("dto {} {{\n", dto.name);

    for field in &dto.fields {
        output.push_str(&format!("    {}: {}\n", field.name, format_type(&field.ty)));
    }

    output.push_str("}\n");
    output
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
    let mut output = format!("service {} {{\n", service.name);

    for method in &service.methods {
        for ann in &method.annotations {
            output.push_str(&format!(
                "    @{}({})\n",
                ann.name,
                ann.args
                    .iter()
                    .map(|a| format!("\"{}\"", a))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }

        let return_type = method
            .output
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or("void");

        output.push_str(&format!(
            "    {}({}) -> {}\n",
            method.name, method.input, return_type
        ));
    }

    output.push_str("}\n");
    output
}
