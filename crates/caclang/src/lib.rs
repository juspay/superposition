use std::{
    collections::{BTreeMap, HashMap},
    str::FromStr,
};
mod helpers;

use anyhow::anyhow;
use derive_more::Deref;
use evalexpr::Node;
use helpers::{compute_cac_hash, extract_section, parse_and_validate_ctx};
use regex::Regex;
use strum_macros::EnumString;
use toml::Table;

pub type HashMapContext = evalexpr::HashMapContext;

#[derive(Debug, Clone, EnumString, PartialEq)]
pub enum DataType {
    #[strum(ascii_case_insensitive)]
    String,
    #[strum(ascii_case_insensitive)]
    Number,
    #[strum(ascii_case_insensitive)]
    Boolean,
}
#[derive(Debug, Clone)]
pub struct Context {
    pub context: String,
    pub expr: Node,
    pub overrides: HashMap<String, String>,
    pub calculated_priority: u64,
}

impl Context {
    pub fn from(
        dimensions: &Dimensions,
        default_config: &Table,
        ctx: String,
        o: &Table,
    ) -> anyhow::Result<Self> {
        let (expr, calculated_priority) = parse_and_validate_ctx(dimensions, &ctx)?;
        let mut overrides = HashMap::new();
        for (config_key, ov) in o.into_iter() {
            default_config.get(config_key).ok_or(anyhow!("The override key {config_key} for the context {ctx} is not present in default-configs section"))?;
            let ove = ov.as_str().ok_or(anyhow!("Invalid syntax for override for the context {ctx} and config key {config_key}"))?.to_string();
            overrides.insert(config_key.into(), ove);
        }
        Ok(Self {
            context: ctx,
            expr,
            overrides,
            calculated_priority,
        })
    }
}

#[derive(Debug, Clone, Deref)]
pub struct Contexts(pub HashMap<String, Context>);

impl Contexts {
    pub fn from(
        dimensions: &Dimensions,
        default_config: &Table,
        value: Table,
    ) -> anyhow::Result<Self> {
        let mut items = HashMap::new();
        for (ctx, overrides) in value.into_iter() {
            let ctx_hash = compute_cac_hash(&ctx)?;
            if items.contains_key(&ctx_hash) {
                let collided_ctx: &Context = items.get(&ctx_hash).ok_or(anyhow!(""))?;
                return Err(anyhow!(
                    "{ctx} is a logical duplicate of {}",
                    collided_ctx.context
                ));
            }
            let overrides = overrides
                .as_table()
                .ok_or(anyhow!("invalid overrides provided for {ctx}"))?;
            items.insert(
                ctx_hash,
                Context::from(dimensions, default_config, ctx, overrides)?,
            );
        }
        Ok(Contexts(items))
    }
}

#[derive(Debug, Clone)]
pub struct DimensionProperties {
    pub data_type: DataType,
    pub pattern: Regex,
    pub priority: u64,
}

impl DimensionProperties {
    fn from(value: &Table) -> anyhow::Result<Self> {
        let data_type = DataType::from_str(
            value
                .get("type")
                .ok_or(anyhow!("type is missing from dimension"))?
                .as_str()
                .ok_or(anyhow!(
                    "Invalid type, allowed values are string, number and boolean"
                ))?,
        )
        .unwrap();
        let pattern = if let Some(p) = value.get("pattern") {
            Regex::new(p.as_str().ok_or(anyhow!("Invalid regex expression"))?)?
        } else {
            Regex::new(".*")?
        };
        let priority = value
            .get("priority")
            .ok_or(anyhow!("Priority field is missing"))?
            .as_integer()
            .ok_or(anyhow!("invalid value for priority"))? as u64;
        Ok(Self {
            data_type,
            pattern,
            priority,
        })
    }
}

#[derive(Debug, Clone, Deref)]
pub struct Dimensions(pub HashMap<String, DimensionProperties>);

impl Dimensions {
    pub fn from(value: Table) -> anyhow::Result<Self> {
        let mut dimension_map = HashMap::new();
        for (dimension, props) in value.into_iter() {
            let properties: &Table = props.as_table().unwrap();
            dimension_map.insert(dimension, DimensionProperties::from(properties)?);
        }
        Ok(Dimensions(dimension_map))
    }
}

#[derive(Debug, Clone)]
pub struct ContextAwareConfig {
    pub dimensions: Dimensions,
    pub default_config: Table,
    pub contexts: Contexts,
}
impl ContextAwareConfig {
    pub fn parse(config: String) -> anyhow::Result<Self> {
        let mut cac: Table = config.parse::<Table>()?;
        let dimensions = Dimensions::from(extract_section(&mut cac, "dimensions")?)?;
        let default_config = extract_section(&mut cac, "default-configs")?;
        let contexts = Contexts::from(&dimensions, &default_config, cac)?;
        Ok(Self {
            dimensions,
            default_config,
            contexts,
        })
    }

    pub fn get_config(
        &self,
        key: &'static str,
        context: &HashMapContext,
    ) -> anyhow::Result<String> {
        let mut sorted_map: BTreeMap<u64, &HashMap<String, String>> = BTreeMap::new();
        for (_, ctx) in self.contexts.iter() {
            let eval = ctx.expr.eval_with_context(context);
            if eval.is_ok() && eval?.as_boolean()? {
                log::debug!("============================================");
                log::debug!("context {}", ctx.context);
                log::debug!("Expression {:?}", ctx.expr.to_string());
                log::debug!("overrides {:?}", ctx.overrides);
                log::debug!("priority {:?}", ctx.calculated_priority);
                log::debug!("============================================");
                sorted_map.insert(ctx.calculated_priority, &ctx.overrides);
            }
        }
        let mut fin_override: HashMap<String, String> = HashMap::new();
        for (_, ov) in sorted_map.into_iter() {
            fin_override.extend(ov.to_owned());
        }

        Ok(fin_override
            .get(key)
            .ok_or(anyhow!(
                "{key} not found in resolved contexts {:?}",
                fin_override
            ))?
            .to_owned())
    }
}
