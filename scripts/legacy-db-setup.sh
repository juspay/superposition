DB_URL=$1

cp -r "crates/context_aware_config/migrations/." "crates/context_aware_config/cac_v1_migrations"
find "crates/context_aware_config/cac_v1_migrations" -name "up.sql" -exec sed -i'' "s/public/cac_v1/g" {} \;

find "crates/context_aware_config/cac_v1_migrations" -name "up.sql" -exec cat {} \;

xargs  cp -r "crates/experimentation_platform/migrations/." "crates/experimentation_platform/cac_v1_migrations"
find "crates/experimentation_platform/cac_v1_migrations" -name "up.sql" -exec sed -i'' "s/public/cac_v1/g" {} \;

find "crates/experimentation_platform/cac_v1_migrations" -name "up.sql" -exec cat {} \;

find "crates/context_aware_config/cac_v1_migrations" -name "up.sql" -exec psql "$DB_URL" -f {} \;
find "crates/experimentation_platform/cac_v1_migrations" -name "up.sql" -exec psql "$DB_URL" -f {} \;

rm -rf "crates/context_aware_config/cac_v1_migrations"
rm -rf "crates/experimentation_platform/cac_v1_migrations"