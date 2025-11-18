#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
k
/**
 * # Safety
 *
 * Caller ensures that `ebuf` is a sufficiently long buffer to store the
 * error message.
 */
char *core_get_resolved_config(const char *default_config_json,
                               const char *contexts_json,
                               const char *overrides_json,
                               const char *dimensions,
                               const char *query_data_json,
                               const char *merge_strategy_str,
                               const char *filter_prefixes_json,
                               const char *experimentation_json,
                               char *ebuf);

/**
 * # Safety
 *
 * Caller ensures that `ebuf` is a sufficiently long buffer to store the
 * error message.
 */
char *core_get_resolved_config_with_reasoning(const char *default_config_json,
                                              const char *contexts_json,
                                              const char *overrides_json,
                                              const char *dimensions,
                                              const char *query_data_json,
                                              const char *merge_strategy_str,
                                              const char *filter_prefixes_json,
                                              const char *experimentation_json,
                                              char *ebuf);

int32_t core_test_connection(void);

/**
 * # Safety
 *
 * This function is unsafe because:
 * - `s` must be a valid pointer to a C string previously allocated by this library
 * - `s` must not be null
 * - The caller must ensure `s` is not used after this function is called
 * - Double-free will cause undefined behavior
 */
void core_free_string(char *s);

/**
 * # Safety
 *
 * Caller ensures that `ebuf` is a sufficiently long buffer to store the
 * error message.
 */
char *core_get_applicable_variants(const char *experiments_json,
                                   const char *experiment_groups_json,
                                   const char *dimensions,
                                   const char *query_data_json,
                                   const char *identifier,
                                   const char *filter_prefixes_json,
                                   char *ebuf);
