ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS encryption_key TEXT NOT NULL DEFAULT '',
ADD COLUMN IF NOT EXISTS key_rotated_at TIMESTAMPTZ;

CREATE TABLE IF NOT EXISTS public.secrets (
    name VARCHAR(50) PRIMARY KEY,
    encrypted_value TEXT NOT NULL,
    description TEXT NOT NULL,
    change_reason TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_modified_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    created_by VARCHAR(200) NOT NULL,
    last_modified_by VARCHAR(200) NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_secrets_created_at ON public.secrets(created_at);
CREATE INDEX IF NOT EXISTS idx_secrets_last_modified_at ON public.secrets(last_modified_at);

CREATE TRIGGER secrets_audit AFTER INSERT OR DELETE OR UPDATE ON public.secrets FOR EACH ROW EXECUTE FUNCTION public.event_logger();
