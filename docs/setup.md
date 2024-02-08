# Setup Instructions for Superposition Platform

This document outlines the setup process for the `Superposition Platform`.

## Prerequisites

* Docker installed and running.
* Docker Compose installed.
* [Follow installation instructions at Zero To Nix ↗](https://zero-to-nix.com/start/install) 

# Setup Steps

## Start docker daemon
```bash
 open --background -a Docker
 ```
## Shutdown Local Postgres

### For HomeBrew: 
```bash
 # Check if service is running
 brew services
 # If postgres is running, stop it via brew
 brew services stop postgresql@<your_postgres_version>
```
## Clone the repository 
```bash
 git clone ssh://git@ssh.bitbucket.juspay.net/picaf/context-aware-config.git
 cd context-aware-config
```
## Build And Run
```bash
 nix develop
 make setup
 make run 
```
## Check Installation

### Check logs
 ```bash
 {"level":"INFO","service":"context-aware-config","timestamp":"2023-08-14T08:08:20.291Z","value":"starting 5 workers"}
 {"level":"INFO","service":"context-aware-config","timestamp":"2023-08-14T08:08:20.292Z","value":"Actix runtime found; starting in Actix runtime"}
```
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

