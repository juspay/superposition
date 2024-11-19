#!/usr/bin/env bash
shopt -s extglob

KEEP_SCHEMA_SQL=0

while getopts t:d:k flag
do
    case "${flag}" in
        t) TENANT=${OPTARG};;
        d) DB_URL=${OPTARG};;
        k) KEEP_SCHEMA_SQL=1;;
    esac
done

if [ -z ${TENANT} ] || [ -z ${DB_URL} ]; then
    echo "tenant name (-t) and db_url (-d) parameters are mandatory parameters to create a tenant"
    exit -
fi

echo "Tenant ID ==> $TENANT"
echo "DB URL ==> $DB_URL"

# Creating schemas
CAC_SCHEMA="${TENANT}_cac"
EXP_SCHEMA="${TENANT}_experimentation"

function create_schema() {
    service=$1
    schema=$2

    rm -f ${schema}.sql

    for f in $(find "crates/${service}_db_config/migrations" -name "up.sql" | grep -v "diesel_initial_setup" | sort)
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
    if [ $? -ne 0 ]; then
        echo "Could not execute ${schema}.sql in postgres, dumping it below"
        echo "=======================================";
        cat ${schema.sql}
        echo "=======================================";
    fi
    if [ ${KEEP_SCHEMA_SQL} -ne 1 ]; then
        # remove file to keep the repository clean if keep argument is not sent
        rm -f ${schema}.sql
    fi
}

create_schema "cac" $CAC_SCHEMA
create_schema "experimentation" $EXP_SCHEMA
psql "$DB_URL" -c "INSERT INTO $CAC_SCHEMA.dimensions (dimension, priority, created_at, created_by, schema, function_name) VALUES ('variantIds', 1, CURRENT_TIMESTAMP, 'user@example.com', '{\"type\": \"string\",\"pattern\": \".*\"}'::json, null);"

shopt -u extglob
