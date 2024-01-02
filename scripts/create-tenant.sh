#!/usr/bin/env bash
TENANT=$1
DB_URL=$2

echo "Tenant ID ==> $TENANT"
echo "DB URL ==> $DB_URL"

# Creating schemas
CAC_SCHEMA="${TENANT}_cac"
EXP_SCHEMA="${TENANT}_experimentation"

cp -r "crates/context-aware-config/migrations/." "crates/context-aware-config/${TENANT}_migrations"
find "crates/context-aware-config/${TENANT}_migrations" -name "up.sql" | xargs sed -i'' "s/public/${CAC_SCHEMA}/g"

cp -r "crates/experimentation-platform/migrations/." "crates/experimentation-platform/${TENANT}_migrations"
find "crates/experimentation-platform/${TENANT}_migrations" -name "up.sql" | xargs sed -i'' "s/public/${EXP_SCHEMA}/g"

echo "Creating $CAC_SCHEMA"
find "crates/context-aware-config/${TENANT}_migrations" -name "up.sql" -exec psql "$DB_URL" -f {} \;

echo "Creating $EXP_SCHEMA"
find "crates/experimentation-platform/${TENANT}_migrations" -name "up.sql" -exec psql "$DB_URL" -f {} \;

rm -rf "crates/context-aware-config/${TENANT}_migrations"
rm -rf "crates/experimentation-platform/${TENANT}_migrations"