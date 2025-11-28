use actix_web::{
    web::{self, Json, ServiceConfig},
    HttpResponse,
};
use async_openai::{
    config::OpenAIConfig,
    types::{k
        ChatCompletionRequestMessage, ChatCompletionRequestSystemMessageArgs,
        ChatCompletionRequestUserMessageArgs, CreateChatCompletionRequestArgs,
    },
    Client,
};
use log::{error, info};
use serde_json;
use service_utils::service::types::AppState;

use super::types::{GenerateRequest, GenerateResponse};

pub fn configure(cfg: &mut ServiceConfig) {
    cfg.service(web::resource("/generate").route(web::post().to(generate_handler)));
}

async fn generate_handler(
    request: Json<GenerateRequest>,
    state: web::Data<AppState>,
) -> HttpResponse {
    info!("AI generation request: {:?}", request);

    // Get OpenAI configuration from app state
    let api_key = match &state.ai_config.api_key {
        Some(key) => key.clone(),
        None => {
            error!("OPENAI_API_KEY not configured");
            return HttpResponse::InternalServerError().json(serde_json::json!({
                "error": "OpenAI API key not configured"
            }));
        }
    };

    // Support custom OpenAI base URL for OpenAI-compatible APIs
    let config = if let Some(base_url) = &state.ai_config.base_url {
        OpenAIConfig::new().with_api_key(api_key).with_api_base(base_url)
    } else {
        OpenAIConfig::new().with_api_key(api_key)
    };

    let client = Client::with_config(config);

    // Get model from app state
    let model = &state.ai_config.model;

    // Build the prompt
    let system_prompt = request.generation_type.get_system_prompt();
    let user_prompt = if let Some(context) = &request.context {
        format!(
            "Additional context: {}\n\nDescription: {}",
            context, request.description
        )
    } else {
        request.description.clone()
    };

    // Create messages
    let messages = vec![
        ChatCompletionRequestMessage::System(
            ChatCompletionRequestSystemMessageArgs::default()
                .content(system_prompt)
                .build()
                .unwrap(),
        ),
        ChatCompletionRequestMessage::User(
            ChatCompletionRequestUserMessageArgs::default()
                .content(user_prompt)
                .build()
                .unwrap(),
        ),
    ];

    // Create chat completion request
    let request_builder = CreateChatCompletionRequestArgs::default()
        .model(model)
        .messages(messages)
        .temperature(0.7)
        .max_tokens(2048_u32)
        .build();

    let chat_request = match request_builder {
        Ok(req) => req,
        Err(e) => {
            error!("Failed to build chat completion request: {}", e);
            return HttpResponse::InternalServerError().json(serde_json::json!({
                "error": "Failed to build request"
            }));
        }
    };

    // Make the API call
    match client.chat().create(chat_request).await {
        Ok(response) => {
            if let Some(choice) = response.choices.first() {
                if let Some(content) = &choice.message.content {
                    let generated_code = content.trim().to_string();
                    info!("Generated code successfully");
                    HttpResponse::Ok().json(GenerateResponse { generated_code })
                } else {
                    error!("No content in response");
                    HttpResponse::InternalServerError().json(serde_json::json!({
                        "error": "No content generated"
                    }))
                }
            } else {
                error!("No choices in response");
                HttpResponse::InternalServerError().json(serde_json::json!({
                    "error": "No response from AI"
                }))
            }
        }
        Err(e) => {
            error!("OpenAI API error: {}", e);
            HttpResponse::InternalServerError().json(serde_json::json!({
                "error": format!("AI generation failed: {}", e)
            }))
        }
    }
}
