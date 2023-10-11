DB_URL=$1

cp -r "crates/context-aware-config/migrations/." "crates/context-aware-config/cac_v1_migrations"
find "crates/context-aware-config/cac_v1_migrations" -name "up.sql" -exec sed -i'' "s/public/cac_v1/g" {} \;

find "crates/context-aware-config/cac_v1_migrations" -name "up.sql" -exec cat {} \;

xargs  cp -r "crates/experimentation-platform/migrations/." "crates/experimentation-platform/cac_v1_migrations"
find "crates/experimentation-platform/cac_v1_migrations" -name "up.sql" -exec sed -i'' "s/public/cac_v1/g" {} \;

find "crates/experimentation-platform/cac_v1_migrations" -name "up.sql" -exec cat {} \;

find "crates/context-aware-config/cac_v1_migrations" -name "up.sql" -exec psql "$DB_URL" -f {} \;
find "crates/experimentation-platform/cac_v1_migrations" -name "up.sql" -exec psql "$DB_URL" -f {} \;

rm -rf "crates/context-aware-config/cac_v1_migrations"
rm -rf "crates/experimentation-platform/cac_v1_migrations"