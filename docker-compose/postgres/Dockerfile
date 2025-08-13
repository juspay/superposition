FROM public.ecr.aws/docker/library/postgres:15-alpine

COPY ./db_init.sql /docker-entrypoint-initdb.d/db_init.sql
CMD ["postgres", "-c", "log_statement=all"]
