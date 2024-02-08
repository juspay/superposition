#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct Arc_Client Arc_Client;

int last_error_length(void);

const char *last_error_message(void);

void free_string(char *s);

int new_client(const char *tenant,
               bool update_periodically,
               unsigned long update_frequency,
               const char *hostname);

void start_polling_update(const char *tenant);

void free_client(struct Arc_Client *ptr);

struct Arc_Client *get_client(const char *tenant);

const char *cac_eval(struct Arc_Client *client, const char *query);

const char *get_last_modified(struct Arc_Client *client);

const char *get_config(struct Arc_Client *client);
