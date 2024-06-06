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
cargo run
```
