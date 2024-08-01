Set directory path that contains superposition object files in <span style="color: red" > SUPERPOSITION_LIB_PATH </span> env variable;

Set directory path that contains C header file <span style="color: red" > SUPERPOSITION_INCLUDE_PATH </span> env variable;

<span style="color: red" > export CGO_CFLAGS="-I${SUPERPOSITION_INCLUDE_PATH}" </span> (Set this flag in order to know go where to pick C header file from).

## [<u> CAC Client </u>](./cacclient/main.go)

* #### <span style="color: red" > export CGO_LDFLAGS="-L${SUPERPOSITION_LIB_PATH} -lcac_client" </span> (Set this flag in order to know go where to pick superposition object files from).
1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libcac_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libcac_client.so
    *  <span style="color: orange" >For Linux </span> ->  libcac_client.dll
3. This run CAC CLient in two thread one is main thread another is worker thread.
4. Polling updates for config are done on different thread. ([ref](./cacclient/main.go#50)).


## [<u> Experimentation Client </u>](./expclient/main.go)

* #### <span style="color: red" > export CGO_LDFLAGS="-L${SUPERPOSITION_LIB_PATH} -lexperimentation_client" </span> (Set this flag in order to know go where to pick superposition object files from).
1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libexperimentation_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libexperimentation_client.so
    *  <span style="color: orange" >For Linux </span> ->  libexperimentation_client.dll
3. This run Experimentation CLient in two thread one is main thread another is worker thread.
4. Polling updates for experiments are done on different thread. ([ref](./expclient/main.go#55)).


## [<u> Test </u>](./main.go)

* #### <span style="color: red" > export CGO_LDFLAGS="-L${SUPERPOSITION_LIB_PATH} -lcac_client -lexperimentation_client" </span> (Set this to run sample project)
1. To test this sample project follow below steps.
    * Run superposition client.
    * Run <u> **go run main.go** </u>.
2. By Default this sample code uses [dev](./main.go#L11) tenant.
3. By Default this sample code assumes superposition is running on [8080](./main.go#L13) port.
3. By Default this sample code polls superposition every [1 second](./main.go#L12) port.
4. This sample code creates both [CAC CLient](./main.go#15) and [Experimentation Client](./main.go#20) with above default values.