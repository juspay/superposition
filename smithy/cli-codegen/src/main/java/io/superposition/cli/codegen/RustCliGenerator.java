package io.superposition.cli.codegen;

import software.amazon.smithy.build.FileManifest;
import software.amazon.smithy.model.Model;

import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Orchestrates Rust CLI code generation from the Smithy model.
 * Generates a complete Rust crate with clap-based CLI commands.
 */
public final class RustCliGenerator {

    private final Model model;
    private final CliCodegenConfig config;

    public RustCliGenerator(Model model, CliCodegenConfig config) {
        this.model = model;
        this.config = config;
    }

    /**
     * Generates all files for the CLI crate.
     */
    public void generate(FileManifest manifest) {
        ModelWalker walker = new ModelWalker(model, config.getService());
        List<ResourceInfo> resources = walker.walk();

        // Generate Cargo.toml
        manifest.writeFile(
                Path.of(config.getOutputCrate(), "Cargo.toml"),
                generateCargoToml());

        // Generate src/main.rs
        manifest.writeFile(
                Path.of(config.getOutputCrate(), "src", "main.rs"),
                generateMainRs(resources));

        // Generate src/output.rs
        manifest.writeFile(
                Path.of(config.getOutputCrate(), "src", "output.rs"),
                generateOutputRs());

        // Generate src/client.rs
        manifest.writeFile(
                Path.of(config.getOutputCrate(), "src", "client.rs"),
                generateClientRs());

        // Generate src/commands/mod.rs
        manifest.writeFile(
                Path.of(config.getOutputCrate(), "src", "commands", "mod.rs"),
                generateCommandsModRs(resources));

        // Generate per-resource command files
        for (ResourceInfo resource : resources) {
            manifest.writeFile(
                    Path.of(config.getOutputCrate(), "src", "commands",
                            resource.getModuleName() + ".rs"),
                    generateResourceCommandRs(resource));
        }
    }

    private String generateCargoToml() {
        return """
                [package]
                name = "%s"
                version = "0.1.0"
                edition = "2021"
                description = "CLI for Superposition API — auto-generated from Smithy models"

                [[bin]]
                name = "%s"
                path = "src/main.rs"

                [dependencies]
                clap = { version = "4", features = ["derive", "env"] }
                tokio = { version = "1", features = ["full"] }
                serde = { version = "1", features = ["derive"] }
                serde_json = "1"
                reqwest = { version = "0.12", features = ["json"] }
                anyhow = "1"
                comfy-table = "7"
                """.formatted(config.getOutputCrate(), config.getBinaryName());
    }

    private String generateMainRs(List<ResourceInfo> resources) {
        StringBuilder sb = new StringBuilder();

        sb.append("""
                //! Auto-generated CLI for Superposition API.
                //! Generated from Smithy models by cli-codegen plugin.
                //! DO NOT EDIT — changes will be overwritten on regeneration.

                mod client;
                mod commands;
                mod output;

                use clap::{Parser, Subcommand};
                use client::ApiClient;
                use output::OutputFormat;

                #[derive(Parser)]
                #[command(
                    name = "%s",
                    about = "CLI for the Superposition configuration and experimentation platform",
                    version
                )]
                struct Cli {
                    /// Base URL of the Superposition server
                    #[arg(long, env = "SUPERPOSITION_BASE_URL", default_value = "http://localhost:8080")]
                    base_url: String,

                    /// Authentication token (Bearer)
                    #[arg(long, env = "SUPERPOSITION_TOKEN")]
                    token: Option<String>,

                    /// Workspace identifier
                    #[arg(long, short = 'w', env = "SUPERPOSITION_WORKSPACE")]
                    workspace: Option<String>,

                    /// Organisation identifier
                    #[arg(long, short = 'o', env = "SUPERPOSITION_ORG")]
                    org: Option<String>,

                    /// Output format
                    #[arg(long, value_enum, default_value = "json")]
                    output: OutputFormat,

                    #[command(subcommand)]
                    command: Commands,
                }

                #[derive(Subcommand)]
                enum Commands {
                """.formatted(config.getBinaryName()));

        for (ResourceInfo resource : resources) {
            String pascalName = ArgMapper.toPascalCase(resource.getModuleName());
            String doc = "Manage " + resource.getResourceName() + " resources";
            sb.append("    /// ").append(doc).append("\n");
            sb.append("    #[command(subcommand)]\n");
            sb.append("    ").append(pascalName)
                    .append("(commands::")
                    .append(resource.getModuleName())
                    .append("::")
                    .append(pascalName)
                    .append("Commands),\n");
        }

        sb.append("}\n\n");

        // Main function
        sb.append("""
                #[tokio::main]
                async fn main() -> anyhow::Result<()> {
                    let cli = Cli::parse();
                    let client = ApiClient::new(
                        cli.base_url.clone(),
                        cli.token.clone(),
                        cli.workspace.clone(),
                        cli.org.clone(),
                    );

                    let result = match cli.command {
                """);

        for (ResourceInfo resource : resources) {
            String pascalName = ArgMapper.toPascalCase(resource.getModuleName());
            sb.append("        Commands::").append(pascalName).append("(cmd) => {\n");
            sb.append("            commands::").append(resource.getModuleName())
                    .append("::execute(cmd, &client).await\n");
            sb.append("        }\n");
        }

        sb.append("""
                    };

                    match result {
                        Ok(value) => {
                            output::print_output(&value, &cli.output);
                            Ok(())
                        }
                        Err(e) => {
                            eprintln!("Error: {e:#}");
                            std::process::exit(1);
                        }
                    }
                }
                """);

        return sb.toString();
    }

    private String generateOutputRs() {
        return """
                //! Output formatting for CLI responses.

                use comfy_table::{Table, presets::UTF8_FULL};
                use serde_json::Value;

                #[derive(clap::ValueEnum, Clone, Debug)]
                pub enum OutputFormat {
                    Json,
                    Table,
                    Pretty,
                }

                pub fn print_output(value: &Value, format: &OutputFormat) {
                    match format {
                        OutputFormat::Json => {
                            println!("{}", serde_json::to_string(value).unwrap_or_default());
                        }
                        OutputFormat::Pretty => {
                            println!("{}", serde_json::to_string_pretty(value).unwrap_or_default());
                        }
                        OutputFormat::Table => {
                            print_as_table(value);
                        }
                    }
                }

                fn print_as_table(value: &Value) {
                    match value {
                        Value::Array(arr) => {
                            if arr.is_empty() {
                                println!("(empty)");
                                return;
                            }
                            let mut table = Table::new();
                            table.load_preset(UTF8_FULL);

                            // Use keys from the first object as headers
                            if let Some(Value::Object(first)) = arr.first() {
                                let headers: Vec<&str> = first.keys().map(|k| k.as_str()).collect();
                                table.set_header(headers.clone());

                                for item in arr {
                                    if let Value::Object(obj) = item {
                                        let row: Vec<String> = headers
                                            .iter()
                                            .map(|h| format_cell(obj.get(*h)))
                                            .collect();
                                        table.add_row(row);
                                    }
                                }
                            }
                            println!("{table}");
                        }
                        Value::Object(obj) => {
                            // Single object: key-value table
                            let mut table = Table::new();
                            table.load_preset(UTF8_FULL);
                            table.set_header(vec!["Field", "Value"]);
                            for (key, val) in obj {
                                table.add_row(vec![key.clone(), format_cell(Some(val))]);
                            }
                            println!("{table}");
                        }
                        other => {
                            println!("{}", serde_json::to_string_pretty(other).unwrap_or_default());
                        }
                    }
                }

                fn format_cell(value: Option<&Value>) -> String {
                    match value {
                        None => String::from("-"),
                        Some(Value::Null) => String::from("-"),
                        Some(Value::String(s)) => {
                            if s.len() > 60 {
                                format!("{}...", &s[..57])
                            } else {
                                s.clone()
                            }
                        }
                        Some(Value::Bool(b)) => b.to_string(),
                        Some(Value::Number(n)) => n.to_string(),
                        Some(other) => {
                            let s = serde_json::to_string(other).unwrap_or_default();
                            if s.len() > 60 {
                                format!("{}...", &s[..57])
                            } else {
                                s
                            }
                        }
                    }
                }
                """;
    }

    private String generateClientRs() {
        return """
                //! HTTP client for the Superposition API.

                use anyhow::{Context, Result, bail};
                use reqwest::{Client, Method, RequestBuilder};
                use serde_json::Value;

                pub struct ApiClient {
                    http: Client,
                    base_url: String,
                    token: Option<String>,
                    workspace: Option<String>,
                    org: Option<String>,
                }

                impl ApiClient {
                    pub fn new(
                        base_url: String,
                        token: Option<String>,
                        workspace: Option<String>,
                        org: Option<String>,
                    ) -> Self {
                        Self {
                            http: Client::new(),
                            base_url: base_url.trim_end_matches('/').to_string(),
                            token,
                            workspace,
                            org,
                        }
                    }

                    /// Build a request with common headers applied.
                    pub fn request(&self, method: Method, path: &str) -> RequestBuilder {
                        let url = format!("{}{}", self.base_url, path);
                        let mut req = self.http.request(method, &url);

                        if let Some(ref token) = self.token {
                            req = req.bearer_auth(token);
                        }
                        if let Some(ref ws) = self.workspace {
                            req = req.header("x-workspace", ws);
                        }
                        if let Some(ref org) = self.org {
                            req = req.header("x-org-id", org);
                        }

                        req
                    }

                    /// Execute a request and return the response as JSON.
                    pub async fn execute(&self, req: RequestBuilder) -> Result<Value> {
                        let response = req.send().await.context("Failed to send request")?;
                        let status = response.status();

                        if !status.is_success() {
                            let body = response.text().await.unwrap_or_default();
                            bail!("HTTP {status}: {body}");
                        }

                        // Handle 204 No Content
                        if status == reqwest::StatusCode::NO_CONTENT {
                            return Ok(Value::Null);
                        }

                        let body = response.text().await.context("Failed to read response body")?;
                        if body.is_empty() {
                            return Ok(Value::Null);
                        }

                        serde_json::from_str(&body)
                            .context("Failed to parse response as JSON")
                    }

                    pub fn workspace_or_err(&self) -> Result<&str> {
                        self.workspace
                            .as_deref()
                            .ok_or_else(|| anyhow::anyhow!(
                                "Workspace required. Use --workspace or set SUPERPOSITION_WORKSPACE"
                            ))
                    }

                    pub fn org_or_err(&self) -> Result<&str> {
                        self.org
                            .as_deref()
                            .ok_or_else(|| anyhow::anyhow!(
                                "Organisation required. Use --org or set SUPERPOSITION_ORG"
                            ))
                    }
                }
                """;
    }

    private String generateCommandsModRs(List<ResourceInfo> resources) {
        StringBuilder sb = new StringBuilder();
        sb.append("//! Command modules for each API resource.\n\n");
        for (ResourceInfo resource : resources) {
            sb.append("pub mod ").append(resource.getModuleName()).append(";\n");
        }
        return sb.toString();
    }

    /**
     * Generates a Rust module file for a single resource, containing
     * all its subcommands and their argument structs.
     */
    private String generateResourceCommandRs(ResourceInfo resource) {
        StringBuilder sb = new StringBuilder();
        String pascalName = ArgMapper.toPascalCase(resource.getModuleName());

        sb.append("""
                //! Auto-generated commands for %s.
                //! DO NOT EDIT — changes will be overwritten on regeneration.

                use anyhow::Result;
                use clap::{Args, Subcommand};
                use reqwest::Method;
                use serde_json::Value;

                use crate::client::ApiClient;

                """.formatted(resource.getResourceName()));

        // Generate the subcommand enum
        sb.append("#[derive(Subcommand)]\n");
        sb.append("pub enum ").append(pascalName).append("Commands {\n");

        for (OperationInfo op : resource.getOperations()) {
            String cmdPascal = ArgMapper.toPascalCase(op.getCliCommandName());
            if (!op.getDocumentation().isEmpty()) {
                sb.append("    /// ").append(truncateDoc(op.getDocumentation())).append("\n");
            }
            sb.append("    ").append(cmdPascal).append("(")
                    .append(cmdPascal).append("Args),\n");
        }
        sb.append("}\n\n");

        // Generate Args structs for each operation
        for (OperationInfo op : resource.getOperations()) {
            String cmdPascal = ArgMapper.toPascalCase(op.getCliCommandName());
            sb.append(generateArgsStruct(cmdPascal, op));
        }

        // Generate the execute function
        sb.append("pub async fn execute(cmd: ").append(pascalName)
                .append("Commands, client: &ApiClient) -> Result<Value> {\n");
        sb.append("    match cmd {\n");

        for (OperationInfo op : resource.getOperations()) {
            String cmdPascal = ArgMapper.toPascalCase(op.getCliCommandName());
            sb.append("        ").append(pascalName).append("Commands::")
                    .append(cmdPascal).append("(args) => ");
            sb.append("execute_").append(toSnakeCase(op.getCliCommandName()))
                    .append("(args, client).await,\n");
        }

        sb.append("    }\n");
        sb.append("}\n\n");

        // Generate parse_value helper if any operation builds a JSON body
        boolean needsParseValue = resource.getOperations().stream()
                .anyMatch(op -> !op.hasBodyPayload()
                        && (op.getHttpMethod().equals("POST")
                            || op.getHttpMethod().equals("PUT")
                            || op.getHttpMethod().equals("PATCH")));

        if (needsParseValue) {
            sb.append("""
                /// Parse a string value, attempting JSON first, falling back to string.
                fn parse_value(s: &str) -> Value {
                    serde_json::from_str(s).unwrap_or_else(|_| Value::String(s.to_string()))
                }

                """);
        }

        // Generate individual execute functions
        for (OperationInfo op : resource.getOperations()) {
            sb.append(generateExecuteFunction(op));
        }

        return sb.toString();
    }

    private String generateArgsStruct(String structName, OperationInfo op) {
        StringBuilder sb = new StringBuilder();
        sb.append("#[derive(Args)]\n");
        sb.append("pub struct ").append(structName).append("Args {\n");

        for (ArgMapper.ClapArg arg : op.getArgs()) {
            generateArgField(sb, arg);
        }

        sb.append("}\n\n");
        return sb.toString();
    }

    private void generateArgField(StringBuilder sb, ArgMapper.ClapArg arg) {
        // Documentation
        if (!arg.help.isEmpty()) {
            sb.append("    /// ").append(truncateDoc(arg.help)).append("\n");
        }

        // Clap attribute
        if (arg.binding == ArgMapper.ClapArg.Binding.POSITIONAL) {
            sb.append("    #[arg(");
            if (arg.required) {
                sb.append("required = true");
            }
            sb.append(")]\n");
        } else if (arg.binding == ArgMapper.ClapArg.Binding.BODY) {
            sb.append("    /// JSON body (inline or @filepath)\n");
            sb.append("    #[arg(long = \"body\")]\n");
        } else if (arg.isFlag) {
            sb.append("    #[arg(long = \"").append(arg.cliName).append("\")]\n");
        } else if (arg.multiple) {
            sb.append("    #[arg(long = \"").append(arg.cliName)
                    .append("\", num_args = 1..)]\n");
        } else {
            sb.append("    #[arg(long = \"").append(arg.cliName).append("\"");
            if (!arg.enumValues.isEmpty()) {
                String vals = arg.enumValues.stream()
                        .map(v -> "\"" + v + "\"")
                        .collect(Collectors.joining(", "));
                sb.append(", value_parser = clap::builder::PossibleValuesParser::new([")
                        .append(vals).append("])");
            }
            sb.append(")]\n");
        }

        // Field declaration
        String fieldName = toSnakeCase(arg.fieldName);
        if (arg.required && arg.binding != ArgMapper.ClapArg.Binding.BODY) {
            if (arg.multiple) {
                sb.append("    pub ").append(fieldName).append(": Vec<String>,\n");
            } else {
                sb.append("    pub ").append(fieldName).append(": ")
                        .append(arg.rustType).append(",\n");
            }
        } else {
            if (arg.multiple) {
                sb.append("    pub ").append(fieldName).append(": Vec<String>,\n");
            } else if (arg.isFlag) {
                sb.append("    pub ").append(fieldName).append(": bool,\n");
            } else {
                sb.append("    pub ").append(fieldName).append(": Option<")
                        .append(arg.rustType).append(">,\n");
            }
        }
        sb.append("\n");
    }

    private String generateExecuteFunction(OperationInfo op) {
        StringBuilder sb = new StringBuilder();
        String fnName = "execute_" + toSnakeCase(op.getCliCommandName());
        String structName = ArgMapper.toPascalCase(op.getCliCommandName()) + "Args";
        String method = switch (op.getHttpMethod()) {
            case "GET" -> "Method::GET";
            case "POST" -> "Method::POST";
            case "PUT" -> "Method::PUT";
            case "PATCH" -> "Method::PATCH";
            case "DELETE" -> "Method::DELETE";
            case "HEAD" -> "Method::HEAD";
            default -> "Method::GET";
        };

        sb.append("async fn ").append(fnName).append("(args: ")
                .append(structName).append(", client: &ApiClient) -> Result<Value> {\n");

        // Build the path, substituting httpLabel args
        String uriPattern = op.getHttpUri();
        List<ArgMapper.ClapArg> labelArgs = op.getArgs().stream()
                .filter(a -> a.binding == ArgMapper.ClapArg.Binding.POSITIONAL)
                .toList();

        if (labelArgs.isEmpty()) {
            sb.append("    let path = \"").append(uriPattern).append("\".to_string();\n");
        } else {
            sb.append("    let path = \"").append(uriPattern).append("\"");
            for (ArgMapper.ClapArg label : labelArgs) {
                String placeholder = "{" + label.fieldName + "}";
                sb.append("\n        .replace(\"").append(placeholder)
                        .append("\", &args.").append(toSnakeCase(label.fieldName)).append(")");
            }
            sb.append(";\n");
        }

        // Build query parameters
        List<ArgMapper.ClapArg> queryArgs = op.getArgs().stream()
                .filter(a -> a.binding == ArgMapper.ClapArg.Binding.NAMED
                        && !a.isFlag
                        && a.binding != ArgMapper.ClapArg.Binding.POSITIONAL)
                .toList();

        sb.append("    let mut req = client.request(").append(method).append(", &path);\n");

        for (ArgMapper.ClapArg qa : queryArgs) {
            String fieldRef = "args." + toSnakeCase(qa.fieldName);
            if (qa.multiple) {
                sb.append("    for v in &").append(fieldRef).append(" {\n");
                sb.append("        req = req.query(&[(\"")
                        .append(qa.fieldName).append("\", v)]);\n");
                sb.append("    }\n");
            } else if (qa.required) {
                sb.append("    req = req.query(&[(\"")
                        .append(qa.fieldName).append("\", &")
                        .append(fieldRef).append(")]);\n");
            } else {
                sb.append("    if let Some(ref v) = ").append(fieldRef).append(" {\n");
                sb.append("        req = req.query(&[(\"")
                        .append(qa.fieldName).append("\", v)]);\n");
                sb.append("    }\n");
            }
        }

        // Handle boolean flags as query params
        List<ArgMapper.ClapArg> flagArgs = op.getArgs().stream()
                .filter(a -> a.isFlag && a.binding == ArgMapper.ClapArg.Binding.NAMED)
                .toList();
        for (ArgMapper.ClapArg fa : flagArgs) {
            String fieldRef = "args." + toSnakeCase(fa.fieldName);
            sb.append("    if ").append(fieldRef).append(" {\n");
            sb.append("        req = req.query(&[(\"")
                    .append(fa.fieldName).append("\", \"true\")]);\n");
            sb.append("    }\n");
        }

        // Handle body payload
        if (op.hasBodyPayload()) {
            ArgMapper.ClapArg bodyArg = op.getArgs().stream()
                    .filter(a -> a.binding == ArgMapper.ClapArg.Binding.BODY)
                    .findFirst()
                    .orElse(null);

            if (bodyArg != null) {
                String fieldRef = "args." + toSnakeCase(bodyArg.fieldName);
                sb.append("    if let Some(ref body_str) = ").append(fieldRef).append(" {\n");
                sb.append("        let body_json = if body_str.starts_with('@') {\n");
                sb.append("            let path = &body_str[1..];\n");
                sb.append("            let content = std::fs::read_to_string(path)\n");
                sb.append("                .map_err(|e| anyhow::anyhow!(\"Failed to read {}: {}\", path, e))?;\n");
                sb.append("            serde_json::from_str(&content)?\n");
                sb.append("        } else {\n");
                sb.append("            serde_json::from_str(body_str)?\n");
                sb.append("        };\n");
                sb.append("        req = req.json(&body_json);\n");
                sb.append("    }\n");
            }
        } else {
            // For POST/PUT/PATCH without explicit body payload, build JSON from non-query args
            if (op.getHttpMethod().equals("POST") || op.getHttpMethod().equals("PUT")
                    || op.getHttpMethod().equals("PATCH")) {
                List<ArgMapper.ClapArg> bodyFields = op.getArgs().stream()
                        .filter(a -> a.binding == ArgMapper.ClapArg.Binding.NAMED
                                && !a.isFlag)
                        .toList();

                if (!bodyFields.isEmpty()) {
                    sb.append("    let mut body = serde_json::Map::new();\n");
                    for (ArgMapper.ClapArg bf : bodyFields) {
                        String fieldRef = "args." + toSnakeCase(bf.fieldName);
                        if (bf.multiple) {
                            sb.append("    if !").append(fieldRef).append(".is_empty() {\n");
                            sb.append("        body.insert(\"").append(bf.fieldName)
                                    .append("\".into(), serde_json::json!(")
                                    .append(fieldRef).append("));\n");
                            sb.append("    }\n");
                        } else if (bf.required) {
                            // Check if value looks like JSON
                            sb.append("    body.insert(\"").append(bf.fieldName)
                                    .append("\".into(), parse_value(&")
                                    .append(fieldRef).append("));\n");
                        } else {
                            sb.append("    if let Some(ref v) = ").append(fieldRef).append(" {\n");
                            sb.append("        body.insert(\"").append(bf.fieldName)
                                    .append("\".into(), parse_value(v));\n");
                            sb.append("    }\n");
                        }
                    }
                    sb.append("    req = req.json(&body);\n");
                }
            }
        }

        sb.append("    client.execute(req).await\n");
        sb.append("}\n\n");

        return sb.toString();
    }

    private static String truncateDoc(String doc) {
        String singleLine = doc.replace("\n", " ").trim();
        if (singleLine.length() > 100) {
            return singleLine.substring(0, 97) + "...";
        }
        return singleLine;
    }

    private static String toSnakeCase(String input) {
        return input
                .replaceAll("([a-z])([A-Z])", "$1_$2")
                .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
                .replaceAll("-", "_")
                .toLowerCase();
    }
}
