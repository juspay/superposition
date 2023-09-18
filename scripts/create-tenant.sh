#!/bin/bash

HOST="localhost"
PORT="5432"
USER="postgres"
PASSWORD="docker"
DATABASE="config"

TENANT=$1
DB_URL=$2

echo "Tenant ID ==> $TENANT"
echo "DB URL ==> $DB_URL"

# Creating schemas
SCHEMA1="${TENANT}_cac"
SCHEMA2="${TENANT}_experimentation"

echo "Creating Schemas ==> $SCHEMA1, $SCHEMA2"

psql "$DB_URL" -c "CREATE SCHEMA ${SCHEMA1}"
psql "$DB_URL" -c "CREATE SCHEMA ${SCHEMA2}"

# Running migrations in created schemas
sed "s/{{schema}}/${SCHEMA1}/g" "$PWD/scripts/cac-init.sql" > $PWD/scripts/cac-init-with-schema.sql
sed "s/{{schema}}/${SCHEMA2}/g" "$PWD/scripts/experimentation-init.sql" > $PWD/scripts/experimentation-init-with-schema.sql

psql "$DB_URL" -f "$PWD/scripts/cac-init-with-schema.sql"
psql "$DB_URL" -f "$PWD/scripts/experimentation-init-with-schema.sql"