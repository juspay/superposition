-- Your SQL goes here
ALTER TABLE public.dimensions ADD COLUMN function_name text NULL;

ALTER TABLE public.dimensions ADD FOREIGN KEY(function_name) REFERENCES public.functions(function_name);

ALTER TABLE public.default_configs ADD COLUMN function_name text NULL;

ALTER TABLE public.default_configs ADD FOREIGN KEY(function_name) REFERENCES public.functions(function_name);