Set directory path that contains superposition object files in <span style="color: red" > SUPERPOSITION_LIB_PATH </span> env variable;

## [<u> CAC Client </u>](./cacclient/CacClient.lua)

1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libcac_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libcac_client.so
    *  <span style="color: orange" >For Linux </span> ->  libcac_client.dll
3. This run CAC CLient in two thread one is main thread another is worker thread.
4. Polling updates for config are done on different thread. ([ref](./cacclient/CacClient.lua#78)).


## [<u> Experimentation Client </u>](./expclient/ExperimentationClient.lua)

1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libexperimentation_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libexperimentation_client.so
    *  <span style="color: orange" >For Linux </span> ->  libexperimentation_client.dll
3. This run Experimentation CLient in two thread one is main thread another is worker thread.
4. Polling updates for experiments are done on different thread. ([ref](./expclient/ExperimentationClient.lua#80)).


## [<u> Test </u>](./main.lua)

1. To test this sample project follow below steps.
    * Run superposition client.
    * Run <u> **luajit main.lua** </u>.
2. By Default this sample code uses [dev](./main.lua#4) tenant.
3. By Default this sample code assumes superposition is running on [8080](./main.lua#6) port.
3. By Default this sample code polls superposition every [1 second](./main.lua#L5) port.
4. This sample code creates both [CAC CLient](./main.lua#12) and [Experimentation Client](./main.lua#8) with above default values.