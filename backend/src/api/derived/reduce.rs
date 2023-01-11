// TODO :: Handle errors with appropriate error message
use std::collections::HashSet;

use actix_web::{
    delete,
    web::{Data, Json},
};

use crate::utils::errors::AppError;

use crate::api::primary::{
    overrides::{delete_override_helper, get_all_overrides},
    context_overrides::fetch_all_ctx_overrides,
    new_contexts::{delete_new_context_helper, fetch_all_new_contexts}
};


use crate::{AppState};

use std::iter::FromIterator;


fn find_diff(all_keys: &Vec<String>, sub_set_keys: &Vec<String>) -> Vec<String> {

    let a_set: HashSet<String> = HashSet::from_iter(sub_set_keys.iter().cloned());
    let mut difference = Vec::new();

    for i in all_keys {
        if !a_set.contains(i) {
            difference.push(i.to_owned());
        }
    }

    difference
}

#[delete("")]
pub async fn reduce_contexts_overrides(input_state: Data<AppState>) -> Result<Json<String>, AppError> {
    let state = &input_state;

    let ctx_overrides = fetch_all_ctx_overrides(state).await?;

    let mapped_context_ids: Vec<String> =
        ctx_overrides
        .iter()
        .map(|i| i.context_id.to_owned())
        .collect();

    let mapped_override_ids: Vec<String> =
        ctx_overrides
        .iter()
        .map(|i| i.override_id.to_owned())
        .collect();


    let all_context_ids: Vec<String> =
        fetch_all_new_contexts(state).await?
        .iter()
        .map(|x| x.key.to_owned())
        .collect();
    let all_override_ids: Vec<String> =
        get_all_overrides(state).await?
        .iter()
        .map(|x| x.key.to_owned())
        .collect();

    let extra_context_ids = find_diff(&all_context_ids,  &mapped_context_ids);
    let extra_override_ids = find_diff(&all_override_ids,  &mapped_override_ids);

    for item in extra_context_ids {
        delete_new_context_helper(&state, item).await?;
    }

    for item in extra_override_ids {
        delete_override_helper(&state, item).await?;
    }

    Ok(Json("Okay".to_string()))
}