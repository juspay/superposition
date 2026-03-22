use anyhow::{Context, Result, bail};
use aws_smithy_types::Document;
use clap::Parser;
use rustyline::DefaultEditor;
use serde_json::Value;
use superposition_sdk::{Client, Config};
use tabled::settings::Style;
use tabled::builder::Builder;

// ---------------------------------------------------------------------------
// CLI args
// ---------------------------------------------------------------------------

#[derive(Parser)]
#[command(
    name = "supersql",
    about = "SQL CLI for querying Superposition configs",
    long_about = "Query Superposition configuration using SQL-like syntax.\n\n\
        Examples:\n  \
        select * from myorg.myworkspace\n  \
        select payment_method from myorg.myworkspace where city = 'bangalore'\n  \
        select key1, key2 from org.ws where os = 'android' and merchant = 'ikea'"
)]
struct Cli {
    /// Superposition server URL (e.g. http://localhost:8080)
    #[arg(long, env = "SUPERPOSITION_URL")]
    url: String,

    /// Bearer token for authentication (optional)
    #[arg(long, env = "SUPERPOSITION_TOKEN", default_value = "")]
    token: String,

    /// Execute a single query and exit (non-interactive mode)
    #[arg(short, long)]
    query: Option<String>,
}

// ---------------------------------------------------------------------------
// SQL AST
// ---------------------------------------------------------------------------

#[derive(Debug)]
struct SelectQuery {
    /// Config keys to select – empty vec means SELECT *
    keys: Vec<String>,
    org: String,
    workspace: String,
    /// dimension = value pairs (all ANDed)
    conditions: Vec<(String, String)>,
}

// ---------------------------------------------------------------------------
// SQL parser
// ---------------------------------------------------------------------------

fn parse_query(input: &str) -> Result<SelectQuery> {
    let input = input.trim().trim_end_matches(';');
    let tokens = tokenize(input)?;
    parse_select(&tokens)
}

/// Tokenize the input into words, respecting quoted strings.
fn tokenize(input: &str) -> Result<Vec<String>> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        if ch.is_whitespace() {
            chars.next();
            continue;
        }
        if ch == '\'' || ch == '"' {
            let quote = ch;
            chars.next(); // consume opening quote
            let mut s = String::new();
            loop {
                match chars.next() {
                    Some(c) if c == quote => break,
                    Some(c) => s.push(c),
                    None => bail!("unterminated string literal"),
                }
            }
            tokens.push(s);
        } else if ch == ',' || ch == '=' || ch == '.' {
            tokens.push(ch.to_string());
            chars.next();
        } else {
            let mut word = String::new();
            while let Some(&c) = chars.peek() {
                if c.is_whitespace() || c == ',' || c == '=' || c == '.' {
                    break;
                }
                word.push(c);
                chars.next();
            }
            tokens.push(word);
        }
    }
    Ok(tokens)
}

fn parse_select(tokens: &[String]) -> Result<SelectQuery> {
    let mut pos = 0;

    // expect SELECT
    expect_keyword(tokens, &mut pos, "select")?;

    // parse column list
    let keys = parse_columns(tokens, &mut pos)?;

    // expect FROM
    expect_keyword(tokens, &mut pos, "from")?;

    // parse org.workspace
    let org = next_token(tokens, &mut pos, "org name")?.to_string();
    expect_token(tokens, &mut pos, ".")?;
    let workspace = next_token(tokens, &mut pos, "workspace name")?.to_string();

    // optional WHERE clause
    let conditions = if pos < tokens.len()
        && tokens[pos].eq_ignore_ascii_case("where")
    {
        pos += 1;
        parse_conditions(tokens, &mut pos)?
    } else {
        vec![]
    };

    if pos < tokens.len() {
        bail!(
            "unexpected token '{}' at position {}",
            tokens[pos],
            pos
        );
    }

    Ok(SelectQuery {
        keys,
        org,
        workspace,
        conditions,
    })
}

fn parse_columns(tokens: &[String], pos: &mut usize) -> Result<Vec<String>> {
    let first = next_token(tokens, pos, "column name or *")?;
    if first == "*" {
        return Ok(vec![]);
    }
    let mut cols = vec![first.to_string()];
    while *pos < tokens.len() && tokens[*pos] == "," {
        *pos += 1; // consume comma
        let col = next_token(tokens, pos, "column name")?;
        cols.push(col.to_string());
    }
    Ok(cols)
}

fn parse_conditions(
    tokens: &[String],
    pos: &mut usize,
) -> Result<Vec<(String, String)>> {
    let mut conds = vec![];
    loop {
        let dim = next_token(tokens, pos, "dimension name")?.to_string();
        expect_token(tokens, pos, "=")?;
        let val = next_token(tokens, pos, "dimension value")?.to_string();
        conds.push((dim, val));

        if *pos < tokens.len() && tokens[*pos].eq_ignore_ascii_case("and") {
            *pos += 1;
        } else {
            break;
        }
    }
    Ok(conds)
}

fn expect_keyword(
    tokens: &[String],
    pos: &mut usize,
    kw: &str,
) -> Result<()> {
    let tok = next_token(tokens, pos, kw)?;
    if !tok.eq_ignore_ascii_case(kw) {
        bail!("expected '{}', found '{}'", kw, tok);
    }
    Ok(())
}

fn expect_token(
    tokens: &[String],
    pos: &mut usize,
    expected: &str,
) -> Result<()> {
    let tok = next_token(tokens, pos, expected)?;
    if tok != expected {
        bail!("expected '{}', found '{}'", expected, tok);
    }
    Ok(())
}

fn next_token<'a>(
    tokens: &'a [String],
    pos: &mut usize,
    what: &str,
) -> Result<&'a str> {
    if *pos >= tokens.len() {
        bail!("expected {} but reached end of query", what);
    }
    let tok = &tokens[*pos];
    *pos += 1;
    Ok(tok)
}

// ---------------------------------------------------------------------------
// SDK execution
// ---------------------------------------------------------------------------

fn build_client(cli: &Cli) -> Result<Client> {
    let mut builder = Config::builder()
        .endpoint_url(&cli.url)
        .behavior_version_latest();

    if !cli.token.is_empty() {
        builder = builder.bearer_token(cli.token.clone().into());
    }

    let config = builder.build();
    Ok(Client::from_conf(config))
}

async fn execute_query(client: &Client, query: &SelectQuery) -> Result<()> {
    let mut req = client
        .get_resolved_config()
        .workspace_id(&query.workspace)
        .org_id(&query.org);

    // Set context from WHERE conditions
    for (dim, val) in &query.conditions {
        req = req.context(dim.clone(), Document::String(val.clone()));
    }

    let resp = req
        .send()
        .await
        .context("failed to resolve config from Superposition")?;

    let config = resp.config();

    // Convert Document to serde_json::Value for easier handling
    let json = document_to_value(config);

    display_results(&query.keys, &json);

    Ok(())
}

fn document_to_value(doc: &Document) -> Value {
    match doc {
        Document::Object(map) => {
            let obj: serde_json::Map<String, Value> = map
                .iter()
                .map(|(k, v)| (k.clone(), document_to_value(v)))
                .collect();
            Value::Object(obj)
        }
        Document::Array(arr) => {
            Value::Array(arr.iter().map(document_to_value).collect())
        }
        Document::Number(n) => match n {
            aws_smithy_types::Number::PosInt(i) => Value::Number((*i).into()),
            aws_smithy_types::Number::NegInt(i) => Value::Number((*i).into()),
            aws_smithy_types::Number::Float(f) => serde_json::Number::from_f64(*f)
                .map(Value::Number)
                .unwrap_or(Value::Null),
        },
        Document::String(s) => Value::String(s.clone()),
        Document::Bool(b) => Value::Bool(*b),
        Document::Null => Value::Null,
    }
}

fn display_results(keys: &[String], config: &Value) {
    let map = match config.as_object() {
        Some(m) => m,
        None => {
            println!("{}", config);
            return;
        }
    };

    if map.is_empty() {
        println!("(0 rows)");
        return;
    }

    // Collect entries, filtering to requested keys if any
    let entries: Vec<(&String, &Value)> = if keys.is_empty() {
        map.iter().collect()
    } else {
        keys.iter()
            .filter_map(|k| map.get(k).map(|v| (k, v)))
            .collect()
    };

    if entries.is_empty() {
        println!("(0 rows)");
        return;
    }

    let mut builder = Builder::default();
    builder.push_record(["key", "value"]);
    for (k, v) in &entries {
        let val_str = match v {
            Value::String(s) => s.clone(),
            other => other.to_string(),
        };
        builder.push_record([k.as_str(), &val_str]);
    }

    let mut table = builder.build();
    table.with(Style::rounded());
    println!("{table}");
    println!("({} rows)", entries.len());
}

// ---------------------------------------------------------------------------
// REPL
// ---------------------------------------------------------------------------

async fn run_repl(cli: &Cli) -> Result<()> {
    let client = build_client(cli)?;
    println!("supersql — SQL CLI for Superposition");
    println!("Connected to {}", cli.url);
    println!("Type your query or 'exit' to quit.\n");

    let mut rl = DefaultEditor::new()?;

    loop {
        let readline = rl.readline("supersql> ");
        match readline {
            Ok(line) => {
                let line = line.trim().to_string();
                if line.is_empty() {
                    continue;
                }
                if line.eq_ignore_ascii_case("exit")
                    || line.eq_ignore_ascii_case("quit")
                    || line == "\\q"
                {
                    break;
                }

                let _ = rl.add_history_entry(&line);

                match parse_query(&line) {
                    Ok(query) => {
                        if let Err(e) = execute_query(&client, &query).await {
                            eprintln!("Error: {e:#}");
                        }
                    }
                    Err(e) => {
                        eprintln!("Parse error: {e}");
                    }
                }
            }
            Err(
                rustyline::error::ReadlineError::Interrupted
                | rustyline::error::ReadlineError::Eof,
            ) => {
                break;
            }
            Err(e) => {
                eprintln!("Error: {e}");
                break;
            }
        }
    }

    println!("Bye!");
    Ok(())
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    if let Some(query_str) = &cli.query {
        let client = build_client(&cli)?;
        let query = parse_query(query_str)?;
        execute_query(&client, &query).await
    } else {
        run_repl(&cli).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_select_star() {
        let q = parse_query("select * from myorg.myws").unwrap();
        assert!(q.keys.is_empty());
        assert_eq!(q.org, "myorg");
        assert_eq!(q.workspace, "myws");
        assert!(q.conditions.is_empty());
    }

    #[test]
    fn test_parse_select_keys() {
        let q =
            parse_query("select key1, key2 from org1.ws1").unwrap();
        assert_eq!(q.keys, vec!["key1", "key2"]);
        assert_eq!(q.org, "org1");
        assert_eq!(q.workspace, "ws1");
    }

    #[test]
    fn test_parse_with_where() {
        let q = parse_query(
            "select payment from org.ws where city = 'bangalore' and os = 'android'",
        )
        .unwrap();
        assert_eq!(q.keys, vec!["payment"]);
        assert_eq!(q.conditions.len(), 2);
        assert_eq!(q.conditions[0], ("city".into(), "bangalore".into()));
        assert_eq!(q.conditions[1], ("os".into(), "android".into()));
    }

    #[test]
    fn test_parse_double_quotes() {
        let q = parse_query(
            r#"select key from o.w where dim = "hello world""#,
        )
        .unwrap();
        assert_eq!(q.conditions[0].1, "hello world");
    }

    #[test]
    fn test_parse_trailing_semicolon() {
        let q = parse_query("select * from o.w;").unwrap();
        assert_eq!(q.org, "o");
        assert_eq!(q.workspace, "w");
    }

    #[test]
    fn test_parse_case_insensitive() {
        let q =
            parse_query("SELECT key FROM o.w WHERE dim = 'val' AND dim2 = 'val2'")
                .unwrap();
        assert_eq!(q.keys, vec!["key"]);
        assert_eq!(q.conditions.len(), 2);
    }

    #[test]
    fn test_parse_error_no_from() {
        assert!(parse_query("select *").is_err());
    }

    #[test]
    fn test_parse_error_no_dot() {
        assert!(parse_query("select * from orgws").is_err());
    }
}
