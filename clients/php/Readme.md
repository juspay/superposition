Set directory path that contains superposition object files in <span style="color: red" > SUPERPOSITION_LIB_PATH </span> env variable;
## [<u> CAC Client </u>](./cacclient/client.php)

1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libcac_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libcac_client.so
    *  <span style="color: orange" >For Linux </span> ->  libcac_client.dll
3. This run CAC CLient in two thread one is main thread another is worker thread.
4. Polling updates for config are done on different thread. ([ref](./cacclient/client.php#L63)).


## [<u> Experimentation Client </u>](./expclient/client.php)

1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libexperimentation_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libexperimentation_client.so
    *  <span style="color: orange" >For Linux </span> ->  libexperimentation_client.dll
3. This run Experimentation CLient in two thread one is main thread another is worker thread.
4. Polling updates for experiments are done on different thread. ([ref](./expclient/client.php#L58)).


## [<u> Test </u>](./server.php)

1. To test this sample project follow below steps.
    * Run superposition client.
    * Run <u> **php main.php** </u>.
2. By Default this sample code uses [dev](./server.php#L5) tenant.
3. By Default this sample code assumes superposition is running on [8080](./server.php#L7) port.
3. By Default this sample code polls superposition every [1 second](./server.php#L6) port.
4. This sample code creates both [CAC CLient](./server.php#L9) and [Experimentation Client](./server.php#L10) with above default values.