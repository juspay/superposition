FROM ghcr.io/juspay/superposition:latest as runtime

WORKDIR /app

RUN apt-get update && apt-get install -y libpq5 ca-certificates curl
RUN apt-get install -y postgresql-common
RUN /usr/share/postgresql-common/pgdg/apt.postgresql.org.sh -i -v 17
# Update the package lists:
RUN apt-get update

COPY examples/superposition-demo-app/pg_hba.conf /etc/postgresql/15/main/pg_hba.conf

COPY docker-compose/postgres/db_init.sql /app/db_init.sql

COPY .env.example /app/.env

COPY examples/superposition-demo-app/superposition_demo.sh .
RUN chmod 774 superposition_demo.sh
CMD ["./superposition_demo.sh"]
