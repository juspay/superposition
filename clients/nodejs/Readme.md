Set directory path that contains superposition object files in <span style="color: red" > SUPERPOSITION_LIB_PATH </span> env variable;

## [<u> CAC Client </u>](./cac-client)

1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libcac_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libcac_client.so
    *  <span style="color: orange" >For Linux </span> ->  libcac_client.dll
3. This run CAC CLient in two thread one is main thread another is worker thread.
4. Worker thread is used to do polling updates ([ref](./cac-client/client.ts#L31)).


## [<u> Experimentation Client </u>](./exp-client)

1. This exports a class that exposes functions that internally call rust functions.
2. For Different platform it read different superposition object files.
    *  <span style="color: #808080" >For Mac </span> ->  libexperimentation_client.dylib
    *  <span style="color: #357EC7" >For Windows </span> ->  libexperimentation_client.so
    *  <span style="color: orange" >For Linux </span> ->  libexperimentation_client.dll
3. This run Experimentation CLient in two thread one is main thread another is worker thread.
4. Worker thread is used to do polling updates ([ref](./exp-client/client.ts#L31)).

## [<u> Test </u>](./index.ts)

1. To test this sample project follow below steps.
    * Run superposition client.
    * Run <u> **npm install** </u> (make sure your node version is <span style="color: red"> >18 </span>).
    * Run <u> **npm run test** </u> this will start a server that runs on port 7000.
2. By Default this sample code uses [dev](./index.ts#L11) tenant.
3. By Default this sample code assumes superposition is running on [8080](./index.ts#L12) port.
3. By Default this sample code polls superposition every [1 second](./index.ts#L13) port.
4. This sample code creates both [CAC CLient](./index.ts#L15) and [Experimentation Client](./index.ts#L16) with above default values.