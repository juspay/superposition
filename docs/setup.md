# Setup Instructions for Superposition Platform

This document outlines the setup process for the `Superposition Platform`.

## Installation for Linux/MacOS

- **Install** [rust](https://rustup.rs).

- **Install** [wasm-pack](https://rustwasm.github.io/wasm-pack/installer/)

- **Install Docker**

- **Installing Lib Dependencies & Tools :** <br>
    `libpq`, `openssl`, `libiconv` are required for building the project. `diesel_cli` and `aws cli` is needed for dev.

    For MacOS :
    ```bash
    brew install libpq openssl libiconv awscli
    ```
    Set the PQ_LIB_DIR environment variable for ensuring libpq is picked up by diesel and/or diesel_cli.
    ```bash
    export PQ_LIB_DIR="$(brew --prefix libpq)/lib"
    ```
    `diesel_cli` can be installed using `cargo`.
    ```bash
    cargo install diesel_cli --no-default-features --features postgres
    ```

- **Configure AWS CLI**
    ```bash
    aws configure
    ```
    Use the following values as input: <br>
      - `AWS Access Key ID`: Test <br>
      - `AWS Secret Access Key`: Test <br>
      - `Default region name`: None <br>
      - `Default output format`: None

- **Ensure `make` Command is Installed**
    - For Linux:
      ```bash
      sudo apt-get install build-essential
      ```
    - For MacOS:
      ```bash
      xcode-select --install
      ```

- **Clone the Repository**
    ```bash
    git clone https://github.com/juspay/superposition.git
    cd superposition
    ```

- **Start Docker Daemon**
    - For Linux:
      ```bash
      sudo systemctl start docker
      ```
    - For MacOS:
      ```bash
      open --background -a Docker
      ```

- **Setup Database and Environment Variables**
    ```bash
    make setup
    ```

- **Run Superposition**
    ```bash
    make run
    ```

## Nix Installation

- Install docker
- make sure you have dependencies like libpq, openssl, libiconv,etc installed
- Clone the repository 
    ```bash
    git clone https://github.com/juspay/superposition.git
    cd superposition
    ``` 
- run `nix develop`
- Start docker daemon
    ```bash
    open --background -a Docker
    ```
- setup database and envs
    `make setup`
- Run Superposition
    `make run`


## Check Installation

### Check /health endpoint
```bash
 curl --location 'http://localhost:8080/health'
 # Expected Response : "Health is good :D"
```    

## Creating New Tenants
```bash 
 make tenant TENANT=<tenant_name> 
 # Add the tenant in the TENANTS env variable. For example TENANTS=dev,test,<tenant_name>
 # Stop the server and run: 
 make run
 ```
## Additional Information

### Make Targets
The following targets are available
* `db-init`: Initializes the database.
* `setup`: Sets up the development environment.
* `kill`: Stops all running containers.
* `run`: Runs the application in development mode.
* `ci-test`: Runs unit tests.
* `ci-build`: Builds the Docker image.
* `ci-push`: Pushes the Docker image to a registry.
* `registry-login`: Logs in to a Docker registry.
* `validate-aws-connection`: Validates the AWS connection.
* `validate-psql-connection`: Validates the PostgreSQL connection.

### Environment Variables
| Variable | Description | Default Value |
|---|---|---|
| `TENANTS` | List of Tenants | `dev,test` |
| `DOCKER_DNS` | DNS server to use within the container | `localhost` |

