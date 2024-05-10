# Setup Instructions for Superposition Platform

This document outlines the setup process for the `Superposition Platform`.

## Installation for linux/MacOS

- Install Rust using rustup
    `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
- Install wasm-pack
    `curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh`
- Install docker
- make sure you have dependencies like libpq, openssl, libiconv,etc installed
- make sure `make` command is installed
  - Linux
    ```bash
    sudo apt-get install build-essential
    ```
  - MacOS
    ```bash
    xcode-select --install
    ```
- Clone the repository 
    ```bash
    git clone https://github.com/juspay/superposition.git
    cd superposition
    ```
- Start docker daemon
    - Linux
        ```bash
        sudo systemctl start docker
        ```
    - MacOS
        ```bash
        open --background -a Docker
        ```
- setup database and envs
    `make setup`
- Run Superposition
    `make run`

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
| `ENABLE_TENANT_AND_SCOPE` | Enables multi-tenancy | `true` |
| `TENANTS` | List of Tenants | `dev,test` |
| `DOCKER_DNS` | DNS server to use within the container | `localhost` |

