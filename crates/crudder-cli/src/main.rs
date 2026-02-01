//! Crudder CLI tool.

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use crudder_codegen::{generator_for, PassManager, Target};
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

        /// Target language (rust, typescript, protobuf, sqlx-postgres, sqlx-sqlite)
        #[arg(short, long)]
        target: String,

        /// Output directory
        #[arg(short, long)]
        output: PathBuf,

        /// Feature passes to include (comma-separated)
        /// TypeScript: zod, express, client
        /// Rust: serde, axum, sqlx-postgres, sqlx-sqlite
        #[arg(long, value_delimiter = ',')]
        with: Vec<String>,
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
            with,
        } => run_generate(&input, &target, &output, &with),
        Commands::Check { input } => run_check(&input),
        Commands::Fmt { input, write } => run_fmt(&input, write),
    };

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Builds a PassManager based on target and feature flags.
///
/// Dependencies are automatically included:
/// - Rust: `axum` requires `serde`, `sqlx-*` requires `serde` + `axum`
fn build_pass_manager(target: &str, features: &[String]) -> Result<PassManager, String> {
    let mut pm = PassManager::new();

    match target.to_lowercase().as_str() {
        // New pass-based targets
        "typescript" | "ts" => {
            pm.add(crudder_codegen::typescript::TypeScriptBasePass);

            for feature in features {
                match feature.to_lowercase().as_str() {
                    "zod" => {
                        pm.add(crudder_codegen::typescript::passes::ZodPass);
                    }
                    "express" => {
                        pm.add(crudder_codegen::typescript::passes::ExpressPass);
                    }
                    "client" => {
                        pm.add(crudder_codegen::typescript::passes::ClientPass);
                    }
                    _ => return Err(format!("unknown TypeScript feature: {}", feature)),
                }
            }
        }
        "rust" => {
            pm.add(crudder_codegen::rust::RustBasePass);

            // Normalize features to lowercase for checking
            let features_lower: Vec<String> = features.iter().map(|f| f.to_lowercase()).collect();

            // Determine which passes are needed (including implicit dependencies)
            let needs_sqlx_pg = features_lower.iter().any(|f| matches!(f.as_str(), "sqlx-postgres" | "postgres" | "pg"));
            let needs_sqlx_sqlite = features_lower.iter().any(|f| matches!(f.as_str(), "sqlx-sqlite" | "sqlite"));
            let needs_axum = features_lower.contains(&"axum".to_string()) || needs_sqlx_pg || needs_sqlx_sqlite;
            let needs_serde = features_lower.contains(&"serde".to_string()) || needs_axum;

            // Add passes in dependency order
            if needs_serde {
                pm.add(crudder_codegen::rust::passes::SerdePass);
            }
            if needs_axum {
                pm.add(crudder_codegen::rust::passes::AxumPass);
            }
            if needs_sqlx_pg {
                pm.add(crudder_codegen::rust::passes::SqlxPass::postgres());
            }
            if needs_sqlx_sqlite {
                pm.add(crudder_codegen::rust::passes::SqlxPass::sqlite());
            }

            // Check for unknown features
            for feature in &features_lower {
                match feature.as_str() {
                    "serde" | "axum" | "sqlx-postgres" | "postgres" | "pg" | "sqlx-sqlite" | "sqlite" => {}
                    _ => return Err(format!("unknown Rust feature: {}", feature)),
                }
            }
        }
        // Backward compat: compound targets
        "sqlx-postgres" | "postgres" | "pg" => {
            pm.add(crudder_codegen::rust::RustBasePass);
            pm.add(crudder_codegen::rust::passes::SerdePass);
            pm.add(crudder_codegen::rust::passes::AxumPass);
            pm.add(crudder_codegen::rust::passes::SqlxPass::postgres());
        }
        "sqlx-sqlite" | "sqlite" => {
            pm.add(crudder_codegen::rust::RustBasePass);
            pm.add(crudder_codegen::rust::passes::SerdePass);
            pm.add(crudder_codegen::rust::passes::AxumPass);
            pm.add(crudder_codegen::rust::passes::SqlxPass::sqlite());
        }
        _ => return Err(format!("unknown target: {}", target)),
    }

    Ok(pm)
}

fn run_generate(
    input: &PathBuf,
    target: &str,
    output: &PathBuf,
    features: &[String],
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

    // Check if we should use the new pass-based system or legacy generators
    let use_pass_manager = matches!(
        target.to_lowercase().as_str(),
        "typescript" | "ts" | "rust" | "sqlx-postgres" | "postgres" | "pg" | "sqlx-sqlite" | "sqlite"
    );

    let files = if use_pass_manager {
        let pm = build_pass_manager(target, features).map_err(|e| e)?;
        pm.run(&schema)?
    } else {
        // Fall back to legacy generators for protobuf
        let target: Target = target.parse().map_err(|e: String| e)?;
        let generator = generator_for(target);
        generator.generate(&schema)?
    };

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
