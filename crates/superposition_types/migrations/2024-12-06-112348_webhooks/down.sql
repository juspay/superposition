-- This file should undo anything in `up.sql`
DROP TRIGGER IF EXISTS webhooks_audit ON public.webhooks;
DROP TABLE IF EXISTS public.webhooks;
DROP TYPE IF EXISTS public.http_method;