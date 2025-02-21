-- This file should undo anything in `up.sql`
ALTER TABLE public.config_versions
ADD COLUMN description NOT NULL default '';