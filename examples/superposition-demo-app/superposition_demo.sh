#!/bin/bash

check_postgres() {
    pg_isready -h localhost -p 5432
}

service postgresql restart

while ! check_postgres; do
    echo "Waiting for PostgreSQL to start..."
    sleep 1
done

echo "CREATE DATABASE config; ALTER USER postgres WITH PASSWORD 'docker';" | psql -U postgres

PGPASSWORD='docker' psql --username=postgres -h localhost config < /app/db_init.sql

/app/superposition