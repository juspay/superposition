use anyhow::anyhow;
use evalexpr::{build_operator_tree, Node};
use toml::Table;

use crate::{DataType, Dimensions};

const AND_TOKEN: &str = " and ";
const JOIN_TOKEN: &str = " && ";

pub(crate) fn extract_section(
    item: &mut Table,
    section_name: &'static str,
) -> anyhow::Result<Table> {
    let section = item
        .remove(section_name)
        .ok_or(anyhow!("{section_name} section not found"))?;
    let section = section.as_table().ok_or(anyhow!(
        "The formatting of the {section_name} is incorrect. Please check the docs"
    ))?;
    Ok(section.clone())
}

pub fn compute_cac_hash(ctx: &String) -> anyhow::Result<String> {
    let tokens = ctx.split(AND_TOKEN).collect::<Vec<&str>>();
    let mut final_tokens: Vec<String> = Vec::new();
    for token in tokens.into_iter() {
        let mut segments = token.trim().split(" ");
        let (dimension, op, value) = (
            segments
                .next()
                .ok_or(anyhow!("Dimension not found for {ctx} in rule {token}"))?,
            segments
                .next()
                .ok_or(anyhow!("Operator not found for {ctx} in rule {token}"))?,
            segments
                .next()
                .ok_or(anyhow!("Value not found for {ctx} in rule {token}"))?,
        );
        if op == "IN" {
            let mut list_items =
                value[1..value.len() - 1].split(",").collect::<Vec<&str>>();
            list_items.sort();
            let final_list = list_items.join(",");
            final_tokens.push(format!("{dimension} {op} [{final_list}]"));
        } else {
            final_tokens.push(String::from(token));
        }
    }
    final_tokens.sort();
    let to_be_hashed = final_tokens.join(",").to_string();
    log::debug!("{}", to_be_hashed);
    let hash = blake3::hash(to_be_hashed.as_bytes()).to_string();
    log::debug!("hash: {}", hash);
    Ok(hash)
}

pub(crate) fn parse_and_validate_ctx(
    dimensions: &Dimensions,
    ctx: &String,
) -> anyhow::Result<(Node, u64)> {
    let mut expr_ctx: Vec<String> = Vec::new();
    let mut priority: u64 = 0;
    for rule in ctx.split(AND_TOKEN).into_iter() {
        let parts: Vec<&str> = rule.trim().split_whitespace().collect();
        if parts.len() != 3 {
            return Err(anyhow!("Invalid rule {rule} in context {ctx}"));
        }
        let mut tokens = parts.into_iter();
        let (dimension, operator, value) = (
            tokens
                .next()
                .ok_or(anyhow!("Dimension not found in {rule}"))?,
            tokens
                .next()
                .ok_or(anyhow!("Operator not found in {rule}"))?,
            tokens.next().ok_or(anyhow!("Value not found in {rule}"))?,
        );
        let props = dimensions.get(dimension).ok_or(anyhow!(
            "Dimension {dimension} is not defined in dimensions section"
        ))?;

        let validator = |value| -> anyhow::Result<()> {
            if !props.pattern.is_match(value) {
                return Err(anyhow!("Invalid value for dimension {dimension} in context {ctx}, the pattern defined does not match with the value provided"));
            }
            Ok(())
        };

        let gen_expr =
            |op: &str, dimension: &str, value: &str, data_type: &DataType| -> String {
                match (op, data_type) {
                    ("IN", DataType::String) => {
                        format!("contains(({value}), {dimension})")
                    }
                    ("IN", _) => format!("contains(({value}),{dimension})"),
                    (_, DataType::String) => format!("{dimension} {op} \"{value}\""),
                    (_, _) => format!("{dimension} {op} {value}"),
                }
            };

        let expr_rule = match operator.to_lowercase().as_str() {
            "is" => {
                validator(value)?;
                gen_expr("==", dimension, value, &props.data_type)
            }
            "=" => {
                validator(value)?;
                gen_expr("==", dimension, value, &props.data_type)
            }
            ">=" => {
                validator(value)?;
                gen_expr(">=", dimension, value, &props.data_type)
            }
            ">" => {
                validator(value)?;
                gen_expr(">", dimension, value, &props.data_type)
            }
            "<=" => {
                validator(value)?;
                gen_expr("<=", dimension, value, &props.data_type)
            }
            "<" => {
                validator(value)?;
                gen_expr("<", dimension, value, &props.data_type)
            }
            "not" => {
                validator(value)?;
                gen_expr("!=", dimension, value, &props.data_type)
            }
            "!=" => {
                validator(value)?;
                gen_expr("!=", dimension, value, &props.data_type)
            }
            "in" => {
                let values = &value[1..value.len() - 1];
                let mut items: Vec<String> = Vec::new();
                for item in values.split(',').into_iter() {
                    validator(item)?;
                    if props.data_type == DataType::String {
                        items.push(format!("\"{item}\""));
                    } else {
                        items.push(format!("{item}"));
                    }
                }
                gen_expr("IN", dimension, items.join(",").as_str(), &props.data_type)
            }
            _ => {
                return Err(anyhow!(
                    "Unsupported operation {operator} in rule {rule} in context {ctx}"
                ))
            }
        };
        priority += props.priority;
        expr_ctx.push(expr_rule);
    }
    let expr = build_operator_tree(expr_ctx.join(JOIN_TOKEN).as_str())?;
    Ok((expr, priority))
}
