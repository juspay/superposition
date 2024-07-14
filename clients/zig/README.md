### Pauli

A zig based client SDK for [`superposition`](https://github.com/juspay/superposition) service.

### Pre-requisites

-   As this client leverages `build.zig.zon` file for managing dependencies. Your zig version should atleast satisfy the minimum version mentioned in the `build.zig.zon` file.
-   You should have `superposition` service running on your local machine or on a remote server. (This is optional in the initial development phase, but might be required for testing)
-   The build steps consist of the following steps:
    -   Pulling the superposition repository, this implies that you have `git` installed on your machine and you have access to the repository.
    -   This is followed by the build stage of `superposition`. (required to get the shared library) `rust` and `cargo` should be installed on your machine for this stage to complete.

### Get Started

-   To get started with the client, create your own zig project:
    ```bash
    mkdir my_project
    cd my_project
    zig init-exe
    ```
-   To install the client as a dependency run the following command:
    ```bash
    zig fetch --save <url/path>
    ```
    Here, the URL can be the place where the zig client is hosted or the path to the client on your local machine.
    The url/path should allow the zig build system to discover the `build.zig.zon` file and the paths included within it.
