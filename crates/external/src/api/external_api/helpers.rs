use experimentation_platform::{
  api::experiments::types::{VariantType, Variant}, 
  db::models::Experiment
};

use service_utils::{
  errors::types::Error as err,
  types as app,
};

pub fn fetch_variant_id(
  experiment: Experiment,
  variant: VariantType,
) -> app::Result<String> {
  let experiment_variants: Vec<Variant> = serde_json::from_value(experiment.variants)
      .map_err(|e| {
      log::error!("parsing to variant type failed with err: {e}");
      err::InternalServerErr("".to_string())
  })?;

  for ele in experiment_variants {
    if ele.variant_type == variant {
        return Ok(ele.id);
    }
  }
  log::info!("Failed to fetch variant {:?} id for exp {}", variant, experiment.id);
  return Err(err::InternalServerErr("".to_string()));
}
