Set directory path that contains superposition object files in <span style="color: red" > SUPERPOSITION_LIB_PATH </span> env variable;

## [<u> CAC Client </u>](./cacclient)

1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libcac_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libcac_client.so
    *  <span style="color: orange" >For Linux </span> ->  libcac_client.dll
3. This run CAC CLient in two thread one is main thread another is worker thread.
4. Polling updates for config are done on different thread. ([ref](./cacclient/client.py#L74)).


## [<u> Experimentation Client </u>](./expclient)

1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libexperimentation_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libexperimentation_client.so
    *  <span style="color: orange" >For Linux </span> ->  libexperimentation_client.dll
3. This run Experimentation CLient in two thread one is main thread another is worker thread.
4. Polling updates for experiments are done on different thread. ([ref](./expclient/client.py#L79)).


## [<u> Test </u>](./main.py)

1. To test this sample project follow below steps.
    * Run superposition client.
    * Run <u> **python3 main.py** </u> this will start a server that runs on port 8002.
2. By Default this sample code uses [dev](./main.py#L7) tenant.
3. By Default this sample code assumes superposition is running on [8080](./main.py#L9) port.
3. By Default this sample code polls superposition every [1 second](./main.py#L8) port.
4. This sample code creates both [CAC CLient](./main.py#L11) and [Experimentation Client](./main.py#L10) with above default values.