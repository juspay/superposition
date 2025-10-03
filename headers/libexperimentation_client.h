#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct Arc_Client Arc_Client;

int expt_last_error_length(void);

const char *expt_last_error_message(void);

void expt_free_string(char *s);

int expt_new_client(const char *tenant, unsigned long update_frequency, const char *hostname);

void expt_start_polling_update(const char *tenant);

void expt_free_client(struct Arc_Client *ptr);

struct Arc_Client *expt_get_client(const char *tenant);

char *expt_get_applicable_variant(struct Arc_Client *client,
                                  const char *c_dimensions,
                                  const char *c_context,
                                  const char *identifier);

char *expt_get_satisfied_experiments(struct Arc_Client *client,
                                     const char *c_context,
                                     const char *filter_prefix);

char *expt_get_filtered_satisfied_experiments(struct Arc_Client *client,
                                              const char *c_context,
                                              const char *filter_prefix);

char *expt_get_running_experiments(struct Arc_Client *client);
