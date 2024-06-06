# Superposition Demo App setup instructions

1. Clone the repo
```sh
git clone https://github.com/knutties/superposition-demo-app.git
```

2. Import the database using 
```sh
psql --username=postgres config < data/demo_app_db_dump.sql
```

3. Compile and run the app
```sh
cargo run --bin cac-demo-app
```
or the following to run the version with the surge pricing feature enabled:
```sh
cargo run --bin cac-demo-app-surge-pricing
```

