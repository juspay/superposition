# Context Aware Config #


### How do I get set up? ###

* Clone this repo
* [Install nix](https://zero-to-nix.com/start/install)
* Start docker daemon either through docker desktop or using the command given below
    ```bash
    $ open --background -a Docker
    ```
* Make sure your brew's postgres service is not running

    ```bash
    # Check if service is running
    $ brew services
    # If postgres is running, stop it via brew
    $ brew services stop postgresql@<your_postgres_version>
    $ cd context-aware-config
    ```
* cd to the repo dir
    ```bash
    $ cd context-aware-config
    ```
* run these commands
     ```bash
    $ nix develop
    $ sh setup.sh
    $ make run OR
    $ make run -e DOCKER_DNS=localhost
    ```
* If you get something like this in the output, you are good to go
     ```bash
   {"level":"INFO","service":"context-aware-config","timestamp":"2023-08-14T08:08:20.291Z","value":"starting 5 workers"}
    {"level":"INFO","service":"context-aware-config","timestamp":"2023-08-14T08:08:20.292Z","value":"Actix runtime found; starting in Actix runtime"}
    ```
* You can hit the `/health` endpoint to confirm server is ready to serve
     ```bash
    $ curl --location 'http://localhost:8080/health'
    #Expected Response : "Health is good :D"
    ```
