#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct Arc_Client Arc_Client;

int cac_last_error_length(void);

const char *cac_last_error_message(void);

void cac_free_string(char *s);

int cac_new_client(const char *tenant,
                   unsigned long update_frequency,
                   const char *hostname,
                   const char *cache_max_capacity);

void cac_start_polling_update(const char *tenant);

void cac_free_client(struct Arc_Client *ptr);

struct Arc_Client *cac_get_client(const char *tenant);

const char *cac_get_last_modified(struct Arc_Client *client);

const char *cac_get_config(struct Arc_Client *client,
                           const char *filter_query,
                           const char *filter_prefix);

const char *cac_get_resolved_config(struct Arc_Client *client,
                                    const char *query,
                                    const char *filter_keys,
                                    const char *merge_strategy);

const char *cac_get_default_config(struct Arc_Client *client, const char *filter_keys);
