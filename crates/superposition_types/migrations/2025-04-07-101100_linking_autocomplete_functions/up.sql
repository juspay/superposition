ALTER TABLE public.dimensions ADD COLUMN autocomplete_function_name text NULL;

ALTER TABLE public.dimensions ADD FOREIGN KEY(autocomplete_function_name) REFERENCES public.functions(function_name);

ALTER TABLE public.default_configs ADD COLUMN autocomplete_function_name text NULL;

ALTER TABLE public.default_configs ADD FOREIGN KEY(autocomplete_function_name) REFERENCES public.functions(function_name);