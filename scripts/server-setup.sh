#!/bin/sh

# script to setup

echo "------------Downloading rust-------------"
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

rustup update


echo "-----------Installing brew dependencies--------------"

brew install postgres@12
brew install libpq
brew services start postgresql

echo "-----------Installing cargo dependencies-------"

cargo install diesel_cli --no-default-features --features postgres
cargo install diesel_cli_ext

echo "-------Diesel Setup Started------------"

echo "DATABASE_URL=postgres://$USER:postgres@localhost/config" > ../.env
diesel setup

cargo remove diesel
cargo add diesel --features "diesel/postgres diesel/r2d2 diesel/serde_json"

echo "--------cargo build started------------"

cargo build
echo "--------cargo build in-progress------------"
cargo remove diesel
cargo add diesel --features "diesel/postgres diesel/r2d2 diesel/serde_json diesel/chrono diesel/uuid"
cargo build
echo "--------cargo build finished-------------"

echo "------------mischellaneous---------------"
touch docker-compose/localstack/export_cyphers.sh
