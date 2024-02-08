#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct Arc_Client Arc_Client;

int last_error_length(void);

const char *last_error_message(void);

void free_string(char *s);

int new_client(const char *tenant, unsigned long update_frequency, const char *hostname);

void start_polling_update(const char *tenant);

void free_client(struct Arc_Client *ptr);

struct Arc_Client *get_client(const char *tenant);

char *get_applicable_variant(struct Arc_Client *client, const char *c_context, short toss);

char *get_satisfied_experiments(struct Arc_Client *client, const char *c_context);

char *get_running_experiments(struct Arc_Client *client);

int last_error_length(void);

const char *last_error_message(void);

void free_string(char *s);

int new_client(const char *tenant, unsigned long update_frequency, const char *hostname);

void start_polling_update(const char *tenant);

void free_client(struct Arc_Client *ptr);

struct Arc_Client *get_client(const char *tenant);

char *get_applicable_variant(struct Arc_Client *client, const char *c_context, short toss);

char *get_satisfied_experiments(struct Arc_Client *client, const char *c_context);

char *get_running_experiments(struct Arc_Client *client);
