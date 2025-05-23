// superposition_core.h
#ifndef SUPERPOSITION_CORE_H
#define SUPERPOSITION_CORE_H

#ifdef __cplusplus
extern "C" {
#endif

// Core FFI functions
char* core_get_resolved_config(
    const char* default_config_json,
    const char* contexts_json,
    const char* overrides_json,
    const char* query_data_json,
    const char* merge_strategy_str
);

char* core_get_resolved_config_with_reasoning(
    const char* default_config_json,
    const char* contexts_json,
    const char* overrides_json,
    const char* query_data_json,
    const char* merge_strategy_str
);

char* core_evaluate_experiment(
    const char* experiments_json, 
    const char* variants_json, 
    const char* overrides_json, 
    const char* user_context_json, 
    int toss, 
    const char* filter_prefixes_json
); 

void core_free_string(char* s);
char* core_last_error_message();
int core_last_error_length();

#ifdef __cplusplus
}
#endif

#endif // SUPERPOSITION_CORE_H