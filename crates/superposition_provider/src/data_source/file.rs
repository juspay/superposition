use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use async_trait::async_trait;
use cac_toml::ContextAwareConfig;
use log::{debug, error, info, warn};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use superposition_types::database::models::cac::{DependencyGraph, DimensionType};
use superposition_types::{Cac, Config, Context, Condition, DimensionInfo, ExtendedMap, OverrideWithKeys, Overrides};
use tokio::sync::RwLock;
use uuid::Uuid;

use crate::data_source::{ConfigData, ExperimentData, SuperpositionDataSource};
use crate::types::{Result, SuperpositionError};

/// Options for configuring the FileDataSource
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileDataSourceOptions {
    /// Path to the .cac.toml configuration file
    pub config_path: PathBuf,
    /// Enable file watching for automatic reload on file changes
    pub watch_files: bool,
}

/// File-based data source that reads configuration from CAC TOML files
///
/// This data source reads Context-Aware Configuration from local .cac.toml files.
/// It supports optional file watching for automatic reloading when the file changes.
#[derive(Debug)]
pub struct FileDataSource {
    options: FileDataSourceOptions,
    cached_config: Arc<RwLock<Option<ConfigData>>>,
    _watcher: Option<RecommendedWatcher>,
}

impl FileDataSource {
    /// Create a new file-based data source
    ///
    /// # Arguments
    ///
    /// * `options` - Configuration options including file path and watch settings
    ///
    /// # Returns
    ///
    /// Returns a new FileDataSource instance or an error if initialization fails
    pub fn new(options: FileDataSourceOptions) -> Result<Self> {
        info!(
            "Creating FileDataSource with config path: {:?}",
            options.config_path
        );

        let cached_config: Arc<RwLock<Option<ConfigData>>> = Arc::new(RwLock::new(None));

        // Set up file watcher if enabled
        let watcher = if options.watch_files {
            let config_path = options.config_path.clone();
            let cached_config_clone = cached_config.clone();

            let mut watcher = notify::recommended_watcher(move |res: notify::Result<Event>| {
                match res {
                    Ok(event) => {
                        // Only reload on Modify and Create events
                        if matches!(
                            event.kind,
                            EventKind::Modify(_) | EventKind::Create(_)
                        ) {
                            info!("File change detected, reloading configuration");
                            let config_path = config_path.clone();
                            let cached_config = cached_config_clone.clone();

                            // Spawn async task to reload config
                            tokio::spawn(async move {
                                match Self::load_config_from_file(&config_path) {
                                    Ok(config_data) => {
                                        let mut cache = cached_config.write().await;
                                        *cache = Some(config_data);
                                        info!("Configuration reloaded successfully");
                                    }
                                    Err(e) => {
                                        error!("Failed to reload configuration: {}", e);
                                    }
                                }
                            });
                        }
                    }
                    Err(e) => {
                        error!("File watch error: {}", e);
                    }
                }
            })
            .map_err(|e| {
                SuperpositionError::ConfigError(format!("Failed to create file watcher: {}", e))
            })?;

            // Start watching the file
            watcher
                .watch(&options.config_path, RecursiveMode::NonRecursive)
                .map_err(|e| {
                    SuperpositionError::ConfigError(format!("Failed to watch file: {}", e))
                })?;

            debug!("File watching enabled for: {:?}", options.config_path);
            Some(watcher)
        } else {
            debug!("File watching disabled");
            None
        };

        Ok(Self {
            options,
            cached_config,
            _watcher: watcher,
        })
    }

    /// Load configuration from the CAC TOML file
    fn load_config_from_file(file_path: &PathBuf) -> Result<ConfigData> {
        debug!("Loading config from file: {:?}", file_path);

        // Parse the CAC TOML file for validation
        let _cac = ContextAwareConfig::parse(
            file_path
                .to_str()
                .ok_or_else(|| {
                    SuperpositionError::ConfigError(
                        "Invalid file path: contains invalid UTF-8".to_string(),
                    )
                })?,
        )
        .map_err(|e| SuperpositionError::ConfigError(format!("Failed to parse CAC TOML: {}", e)))?;

        // Convert CAC TOML to superposition Config
        let config = Self::convert_cac_to_config(file_path)?;

        debug!(
            "Loaded config with {} contexts, {} overrides, {} default configs",
            config.contexts.len(),
            config.overrides.len(),
            config.default_configs.len()
        );

        Ok(ConfigData::new(config))
    }

    /// Convert CAC TOML format to superposition Config
    fn convert_cac_to_config(file_path: &PathBuf) -> Result<Config> {
        // Parse the TOML file to access the raw values
        let toml_content = std::fs::read_to_string(file_path)
            .map_err(|e| SuperpositionError::ConfigError(format!("Failed to read TOML file: {}", e)))?;
        let toml_value: toml::Value = toml::from_str(&toml_content)
            .map_err(|e| SuperpositionError::ConfigError(format!("Failed to parse TOML: {}", e)))?;

        // Convert default configs from toml::Value to serde_json::Value
        let default_configs = Self::extract_default_configs(&toml_value)?;

        // Convert dimensions
        let dimensions = Self::extract_dimensions(&toml_value)?;

        // Convert contexts and overrides
        let (contexts, overrides) =
            Self::extract_contexts_and_overrides(&toml_value, &dimensions)?;

        Ok(Config {
            contexts,
            overrides,
            default_configs,
            dimensions,
        })
    }

    /// Extract default configs from TOML value
    fn extract_default_configs(toml_value: &toml::Value) -> Result<Map<String, Value>> {
        let mut default_configs = Map::new();

        if let Some(default_config) = toml_value.get("default-config") {
            if let Some(table) = default_config.as_table() {
                for (key, value) in table {
                    if let Some(val) = value.get("value") {
                        default_configs.insert(key.clone(), Self::convert_toml_to_json(val));
                    }
                }
            }
        }

        Ok(default_configs)
    }

    /// Convert toml::Value to serde_json::Value
    fn convert_toml_to_json(toml_val: &toml::Value) -> Value {
        match toml_val {
            toml::Value::String(s) => Value::String(s.clone()),
            toml::Value::Integer(i) => Value::Number((*i).into()),
            toml::Value::Float(f) => {
                Value::Number(serde_json::Number::from_f64(*f).unwrap_or_else(|| 0.into()))
            }
            toml::Value::Boolean(b) => Value::Bool(*b),
            toml::Value::Array(arr) => {
                Value::Array(arr.iter().map(Self::convert_toml_to_json).collect())
            }
            toml::Value::Table(table) => Value::Object(
                table
                    .iter()
                    .map(|(k, v)| (k.clone(), Self::convert_toml_to_json(v)))
                    .collect(),
            ),
            toml::Value::Datetime(dt) => Value::String(dt.to_string()),
        }
    }

    /// Extract dimensions from TOML value
    fn extract_dimensions(toml_value: &toml::Value) -> Result<HashMap<String, DimensionInfo>> {
        let mut dimensions = HashMap::new();

        if let Some(dims) = toml_value.get("dimensions") {
            if let Some(table) = dims.as_table() {
                let mut position = 1;
                for (key, value) in table {
                    if let Some(schema) = value.get("schema") {
                        let schema_json = Self::convert_toml_to_json(schema);

                        // Convert to ExtendedMap (which wraps Map<String, Value>)
                        let schema_map = if let Value::Object(map) = schema_json {
                            ExtendedMap::from(map)
                        } else {
                            warn!("Invalid schema for dimension {}, using empty schema", key);
                            ExtendedMap::default()
                        };

                        dimensions.insert(
                            key.clone(),
                            DimensionInfo {
                                schema: schema_map,
                                position,
                                dimension_type: DimensionType::Regular {},
                                dependency_graph: DependencyGraph::default(),
                                value_compute_function_name: None,
                            },
                        );
                        position += 1;
                    } else {
                        warn!("Dimension {} has no schema, skipping", key);
                    }
                }
            }
        }

        Ok(dimensions)
    }

    /// Extract contexts and overrides from TOML value
    ///
    /// Converts CAC TOML context expressions (e.g., "$country == 'US' && $platform == 'web'")
    /// into superposition's JSONLogic format
    fn extract_contexts_and_overrides(
        toml_value: &toml::Value,
        dimensions: &HashMap<String, DimensionInfo>,
    ) -> Result<(Vec<Context>, HashMap<String, Overrides>)> {
        let mut contexts = Vec::new();
        let mut overrides_map = HashMap::new();

        if let Some(context_section) = toml_value.get("context") {
            if let Some(table) = context_section.as_table() {
                let mut priority = 1;

                for (expression, override_values) in table {
                    // Generate unique IDs for context and override
                    let context_id = Uuid::new_v4().to_string();
                    let override_id = Uuid::new_v4().to_string();

                    // Convert expression to JSONLogic condition
                    let condition = Self::expression_to_jsonlogic(expression, dimensions)?;

                    // Extract override values
                    if let Some(override_table) = override_values.as_table() {
                        let override_map: Map<String, Value> = override_table
                            .iter()
                            .map(|(k, v)| (k.clone(), Self::convert_toml_to_json(v)))
                            .collect();

                        if !override_map.is_empty() {
                            // Create Context
                            contexts.push(Context {
                                id: context_id,
                                condition,
                                priority,
                                weight: priority, // Using priority as weight for simplicity
                                override_with_keys: OverrideWithKeys::new(override_id.clone()),
                            });

                            // Create Overrides - convert Map to Overrides via Cac<Overrides>
                            match Cac::<Overrides>::try_from(override_map) {
                                Ok(cac_overrides) => {
                                    overrides_map.insert(override_id, cac_overrides.into_inner());
                                }
                                Err(e) => {
                                    warn!("Failed to validate overrides for {}: {}", expression, e);
                                    continue;
                                }
                            }

                            priority += 1;
                        }
                    }
                }
            }
        }

        Ok((contexts, overrides_map))
    }

    /// Convert a CAC TOML expression to JSONLogic format
    ///
    /// This is a simplified parser that converts basic CAC TOML expressions.
    /// For complex expressions, it uses a basic regex-based approach.
    fn expression_to_jsonlogic(
        expression: &str,
        _dimensions: &HashMap<String, DimensionInfo>,
    ) -> Result<Condition> {
        // Simple expression parser using string manipulation
        // This handles basic cases like "$country == 'US'" and compound conditions with && and ||

        let jsonlogic = Self::parse_simple_expression(expression)?;

        // Convert to Condition
        let condition_map = if let Value::Object(map) = jsonlogic {
            map
        } else {
            return Err(SuperpositionError::ConfigError(format!(
                "Failed to convert expression to condition: {}",
                expression
            )));
        };

        Cac::try_from(condition_map)
            .map(|cac_cond| cac_cond.into_inner())
            .map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to create Condition from expression '{}': {}",
                    expression, e
                ))
            })
    }

    /// Simple expression parser for basic CAC TOML expressions
    fn parse_simple_expression(expr: &str) -> Result<Value> {
        let expr = expr.trim();

        // Handle && (AND) operator
        if let Some(pos) = expr.find(" && ") {
            let left = &expr[..pos];
            let right = &expr[pos + 4..];
            return Ok(json!({
                "and": [
                    Self::parse_simple_expression(left)?,
                    Self::parse_simple_expression(right)?
                ]
            }));
        }

        // Handle || (OR) operator
        if let Some(pos) = expr.find(" || ") {
            let left = &expr[..pos];
            let right = &expr[pos + 4..];
            return Ok(json!({
                "or": [
                    Self::parse_simple_expression(left)?,
                    Self::parse_simple_expression(right)?
                ]
            }));
        }

        // Handle comparison operators
        for op in ["==", "!=", ">=", "<=", ">", "<"] {
            if let Some(pos) = expr.find(&format!(" {} ", op)) {
                let left = expr[..pos].trim();
                let right = expr[pos + op.len() + 2..].trim();

                let left_val = Self::parse_value(left)?;
                let right_val = Self::parse_value(right)?;

                return Ok(json!({
                    op: [left_val, right_val]
                }));
            }
        }

        Err(SuperpositionError::ConfigError(format!(
            "Failed to parse expression: {}",
            expr
        )))
    }

    /// Parse a value from string (dimension variable, string literal, number, or boolean)
    fn parse_value(s: &str) -> Result<Value> {
        let s = s.trim();

        // Check if it's a dimension variable (starts with $)
        if s.starts_with('$') {
            return Ok(json!({"var": &s[1..]}));
        }

        // Check if it's a string literal (enclosed in single quotes)
        if s.starts_with('\'') && s.ends_with('\'') {
            return Ok(Value::String(s[1..s.len() - 1].to_string()));
        }

        // Check if it's a boolean
        if s == "true" {
            return Ok(Value::Bool(true));
        }
        if s == "false" {
            return Ok(Value::Bool(false));
        }

        // Try to parse as number
        if let Ok(i) = s.parse::<i64>() {
            return Ok(Value::Number(i.into()));
        }
        if let Ok(f) = s.parse::<f64>() {
            return Ok(Value::Number(
                serde_json::Number::from_f64(f).unwrap_or_else(|| 0.into()),
            ));
        }

        // Default to string
        Ok(Value::String(s.to_string()))
    }
}

#[async_trait]
impl SuperpositionDataSource for FileDataSource {
    async fn fetch_config(&self) -> Result<ConfigData> {
        // Check if we have a cached config
        {
            let cache = self.cached_config.read().await;
            if let Some(config_data) = cache.as_ref() {
                debug!("Returning cached config");
                return Ok(config_data.clone());
            }
        }

        // Load config from file
        info!("Loading config from file: {:?}", self.options.config_path);
        let config_data = Self::load_config_from_file(&self.options.config_path)?;

        // Cache the config
        {
            let mut cache = self.cached_config.write().await;
            *cache = Some(config_data.clone());
        }

        Ok(config_data)
    }

    async fn fetch_experiments(&self) -> Result<Option<ExperimentData>> {
        // File-based data source doesn't support experiments initially
        debug!("Experiments not supported in file-based data source");
        Ok(None)
    }

    fn source_name(&self) -> &str {
        "File"
    }

    fn supports_experiments(&self) -> bool {
        false
    }

    async fn close(&self) -> Result<()> {
        debug!("Closing FileDataSource");
        // Watcher is automatically dropped, no manual cleanup needed
        Ok(())
    }
}
