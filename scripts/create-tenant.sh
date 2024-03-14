#!/usr/bin/env bash
shopt -s extglob

TENANT=$1
DB_URL=$2

echo "Tenant ID ==> $TENANT"
echo "DB URL ==> $DB_URL"

# Creating schemas
CAC_SCHEMA="${TENANT}_cac"
EXP_SCHEMA="${TENANT}_experimentation"

function generate_sql() {
    service=$1
    schema=$2

    rm ${schema}.sql

    for f in $(find "crates/$service/migrations" -name "up.sql" | grep -v "diesel_initial_setup" | sort)
    do
        OLDIFS=$IFS
        IFS=
        sql="$(cat $f | sed "s/public/${schema}/g")"
        echo $sql >> "${schema}.sql"
        IFS=$OLDIFS
    done

    echo "Generated ${schema}.sql"

    echo "Running migrations for $schema"
    psql "$DB_URL" -f ${schema}.sql
}

generate_sql "context-aware-config" $CAC_SCHEMA
generate_sql "experimentation-platform" $EXP_SCHEMA
psql "$DB_URL" -c "INSERT INTO $CAC_SCHEMA.dimensions (dimension, priority, created_at, created_by, schema, function_name) VALUES ('variantIds', 1, CURRENT_TIMESTAMP, 'anon@juspay.in', '{\"type\": \"string\",\"pattern\": \".*\"}'::json, null);"

shopt -u extglob