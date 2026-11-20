ARG SUPERPOSITION_BASE=ghcr.io/juspay/superposition:latest
FROM ${SUPERPOSITION_BASE} as runtime

WORKDIR /app

RUN apt-get update && apt-get install -y libpq5 ca-certificates curl
RUN apt-get install -y postgresql-common
# postgresql-server-dev is intentionally omitted: the demo never compiles C
# extensions (db_init.sql uses SQL functions + uuid-ossp, shipped in
# postgresql-18) and it drags in linux-libc-dev/clang with unfixed kernel CVEs.
RUN apt-get install -y postgresql-18
# Update the package lists:
RUN apt-get update

COPY examples/superposition-demo-app/pg_hba.conf /etc/postgresql/18/main/pg_hba.conf

COPY docker-compose/postgres/db_init.sql /app/db_init.sql

COPY .env.example /app/.env
RUN sed -i '/REDIS_URL/d' /app/.env

COPY examples/superposition-demo-app/superposition_demo.sh .
RUN chmod 774 superposition_demo.sh
CMD ["./superposition_demo.sh"]
