-- use this only when kronos is in library mode

CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS kronos_payload_specs (
    name          TEXT        NOT NULL,
    schema_json   JSONB       NOT NULL,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT pk_kronos_payload_specs PRIMARY KEY (name)
);

CREATE TABLE IF NOT EXISTS kronos_configs (
    name          TEXT        NOT NULL,
    values_json   JSONB       NOT NULL,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT pk_kronos_configs PRIMARY KEY (name)
);

CREATE TABLE IF NOT EXISTS kronos_secrets (
    name              TEXT        NOT NULL,
    encrypted_value   BYTEA       NOT NULL,
    created_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT pk_kronos_secrets PRIMARY KEY (name)
);

CREATE TABLE IF NOT EXISTS kronos_endpoints (
    name              TEXT        NOT NULL,
    endpoint_type     TEXT        NOT NULL,
    payload_spec_ref  TEXT,
    config_ref        TEXT,
    spec              JSONB       NOT NULL,
    retry_policy      JSONB,
    created_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT pk_kronos_endpoints PRIMARY KEY (name),
    CONSTRAINT fk_kronos_endpoints_payload_spec FOREIGN KEY (payload_spec_ref) REFERENCES kronos_payload_specs (name),
    CONSTRAINT fk_kronos_endpoints_config FOREIGN KEY (config_ref) REFERENCES kronos_configs (name),
    CONSTRAINT chk_kronos_endpoint_type CHECK (endpoint_type IN ('HTTP', 'KAFKA', 'REDIS_STREAM', 'INTERNAL'))
);

CREATE INDEX IF NOT EXISTS idx_kronos_endpoints_type ON kronos_endpoints (endpoint_type);

CREATE TABLE IF NOT EXISTS kronos_jobs (
    job_id                TEXT        NOT NULL DEFAULT gen_random_uuid()::TEXT,
    endpoint              TEXT        NOT NULL,
    endpoint_type         TEXT        NOT NULL,
    trigger_type          TEXT        NOT NULL,
    status                TEXT        NOT NULL DEFAULT 'ACTIVE',
    version               BIGINT      NOT NULL DEFAULT 1,
    previous_version_id   TEXT,
    replaced_by_id        TEXT,
    idempotency_key       TEXT,
    input                 JSONB,
    run_at                TIMESTAMPTZ,
    cron_expression       TEXT,
    cron_timezone         TEXT,
    cron_starts_at        TIMESTAMPTZ,
    cron_ends_at          TIMESTAMPTZ,
    cron_next_run_at      TIMESTAMPTZ,
    cron_last_tick_at     TIMESTAMPTZ,
    created_at            TIMESTAMPTZ NOT NULL DEFAULT now(),
    retired_at            TIMESTAMPTZ,
    CONSTRAINT pk_kronos_jobs PRIMARY KEY (job_id),
    CONSTRAINT fk_kronos_jobs_endpoint FOREIGN KEY (endpoint) REFERENCES kronos_endpoints (name),
    CONSTRAINT chk_kronos_trigger_type CHECK (trigger_type IN ('IMMEDIATE', 'DELAYED', 'CRON')),
    CONSTRAINT chk_kronos_job_status CHECK (status IN ('ACTIVE', 'RETIRED')),
    CONSTRAINT chk_kronos_job_endpoint_type CHECK (endpoint_type IN ('HTTP', 'KAFKA', 'REDIS_STREAM', 'INTERNAL'))
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_kronos_jobs_idempotency
    ON kronos_jobs (endpoint, idempotency_key)
    WHERE idempotency_key IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_kronos_jobs_cron_due
    ON kronos_jobs (cron_next_run_at)
    WHERE trigger_type = 'CRON' AND status = 'ACTIVE';

CREATE INDEX IF NOT EXISTS idx_kronos_jobs_endpoint ON kronos_jobs (endpoint, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_kronos_jobs_status   ON kronos_jobs (status,   created_at DESC);

CREATE TABLE IF NOT EXISTS kronos_executions (
    execution_id    TEXT        NOT NULL DEFAULT gen_random_uuid()::TEXT,
    job_id          TEXT        NOT NULL,
    endpoint        TEXT        NOT NULL,
    endpoint_type   TEXT        NOT NULL,
    idempotency_key TEXT,
    status          TEXT        NOT NULL DEFAULT 'PENDING',
    input           JSONB,
    output          JSONB,
    attempt_count   BIGINT      NOT NULL DEFAULT 0,
    max_attempts    BIGINT      NOT NULL DEFAULT 1,
    worker_id       TEXT,
    run_at          TIMESTAMPTZ NOT NULL DEFAULT now(),
    started_at      TIMESTAMPTZ,
    completed_at    TIMESTAMPTZ,
    duration_ms     BIGINT,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT pk_kronos_executions PRIMARY KEY (execution_id),
    CONSTRAINT fk_kronos_executions_job FOREIGN KEY (job_id) REFERENCES kronos_jobs (job_id),
    CONSTRAINT chk_kronos_exec_status CHECK (status IN (
        'PENDING', 'QUEUED', 'RUNNING', 'RETRYING', 'SUCCESS', 'FAILED', 'CANCELLED'
    ))
);

CREATE INDEX IF NOT EXISTS idx_kronos_executions_pickup
    ON kronos_executions (status, run_at ASC)
    WHERE status IN ('QUEUED', 'RETRYING', 'PENDING');

CREATE UNIQUE INDEX IF NOT EXISTS idx_kronos_executions_cron_dedup
    ON kronos_executions (job_id, idempotency_key)
    WHERE idempotency_key IS NOT NULL;

CREATE INDEX IF NOT EXISTS idx_kronos_executions_by_job  ON kronos_executions (job_id,  created_at DESC);
CREATE INDEX IF NOT EXISTS idx_kronos_executions_running ON kronos_executions (status,   started_at)
    WHERE status = 'RUNNING';

CREATE TABLE IF NOT EXISTS kronos_attempts (
    attempt_id      TEXT        NOT NULL DEFAULT gen_random_uuid()::TEXT,
    execution_id    TEXT        NOT NULL,
    attempt_number  BIGINT      NOT NULL,
    status          TEXT        NOT NULL,
    started_at      TIMESTAMPTZ NOT NULL,
    completed_at    TIMESTAMPTZ,
    duration_ms     BIGINT,
    output          JSONB,
    error           JSONB,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT pk_kronos_attempts PRIMARY KEY (attempt_id),
    CONSTRAINT fk_kronos_attempts_execution FOREIGN KEY (execution_id) REFERENCES kronos_executions (execution_id),
    CONSTRAINT uq_kronos_attempts_exec_number UNIQUE (execution_id, attempt_number),
    CONSTRAINT chk_kronos_attempt_status CHECK (status IN ('SUCCESS', 'FAILED'))
);

CREATE INDEX IF NOT EXISTS idx_kronos_attempts_by_execution
    ON kronos_attempts (execution_id, attempt_number ASC);

CREATE TABLE IF NOT EXISTS kronos_execution_logs (
    log_id          TEXT        NOT NULL DEFAULT gen_random_uuid()::TEXT,
    execution_id    TEXT        NOT NULL,
    attempt_number  BIGINT      NOT NULL,
    level           TEXT        NOT NULL,
    message         TEXT        NOT NULL,
    logged_at       TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT pk_kronos_execution_logs PRIMARY KEY (log_id),
    CONSTRAINT fk_kronos_logs_execution FOREIGN KEY (execution_id) REFERENCES kronos_executions (execution_id),
    CONSTRAINT chk_kronos_log_level CHECK (level IN ('DEBUG', 'INFO', 'WARN', 'ERROR'))
);

CREATE INDEX IF NOT EXISTS idx_kronos_logs_by_execution
    ON kronos_execution_logs (execution_id, logged_at ASC);
CREATE INDEX IF NOT EXISTS idx_kronos_logs_by_attempt
    ON kronos_execution_logs (execution_id, attempt_number, logged_at ASC);
