DB_URL=$1

cp -r "crates/cac_db/migrations/." "crates/cac_db/cac_v1_migrations"
find "crates/cac_db/cac_v1_migrations" -name "up.sql" -exec sed -i'' "s/public/cac_v1/g" {} \;

find "crates/cac_db/cac_v1_migrations" -name "up.sql" -exec cat {} \;

xargs  cp -r "crates/experimentation_db/migrations/." "crates/experimentation_db/cac_v1_migrations"
find "crates/experimentation_db/cac_v1_migrations" -name "up.sql" -exec sed -i'' "s/public/cac_v1/g" {} \;

find "crates/experimentation_db/cac_v1_migrations" -name "up.sql" -exec cat {} \;

find "crates/cac_db/cac_v1_migrations" -name "up.sql" -exec psql "$DB_URL" -f {} \;
find "crates/experimentation_db/cac_v1_migrations" -name "up.sql" -exec psql "$DB_URL" -f {} \;

rm -rf "crates/cac_db/cac_v1_migrations"
rm -rf "crates/experimentation_db/cac_v1_migrations"