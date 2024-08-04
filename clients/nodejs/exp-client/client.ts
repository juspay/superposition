import * as ffi from 'ffi-napi';
import * as ref from 'ref-napi';
import * as path from 'path';
import os from 'os';
import { isMainThread, parentPort, Worker } from 'worker_threads';

let platform = os.platform();


let libPathEnc: string | undefined = process.env.SUPERPOSITION_LIB_PATH;

if (libPathEnc == "" || libPathEnc == undefined) {
    throw new Error("SUPERPOSITION_LIB_PATH not found in env");
}

let fileName =
        platform == "darwin" ?
            'libexperimentation_client.dylib' :
            platform == "linux" ?
                'libexperimentation_client.dll' :
                'libexperimentation_client.so'

const libPath = path.join(libPathEnc, fileName);

const refType = ref.types;
const int = refType.int;
const string = refType.CString;
const voidType = refType.void;

// -------------------------------------
// this code is running on Main Thread
const worker = new Worker(__filename);
// -------------------------------------


// ----------------------------------------
// this code runs on worker thread
if (!isMainThread && parentPort) {
    let cac_client: Map < String, ExperimentationClient > = new Map();
    parentPort.on("message", (message) => {
        try {
            message = JSON.parse(message);
            if (message) {
                if (message.event === "startPollingUpdate") {
                    let {
                        tenant,
                        pollingFrequency,
                        cacHostName,
                    } = message;
                    let tenantClient = cac_client.get(tenant);
                    if (tenantClient) {
                        tenantClient.startPollingUpdate();
                    } else {
                        tenantClient = new ExperimentationClient(tenant, pollingFrequency, cacHostName);
                        cac_client.set(tenant, tenantClient);
                        tenantClient.startPollingUpdate();
                    }
                }
            }
        } catch (error) {
            console.log("Error While starting polling Update for experimentation client ", error);
        }
    })
}
// -----------------------------------------

class ExperimentationClient {
    tenant: string | null = null;
    pollingFrequency: number = 10;
    cacHostName: string | null = null;
    delimeter: string = ",";

    rustLib = ffi.Library(libPath, {
        'expt_new_client': [int, [string, int, string]],
        'expt_start_polling_update': [voidType, [string]],
        'expt_get_client': ["pointer", [string]],
        'expt_get_applicable_variant': [string, ["pointer", string, int]],
        'expt_get_satisfied_experiments': [string, ["pointer", string, "pointer"]],
        'expt_get_filtered_satisfied_experiments': [string, ["pointer", string, "pointer"]],
        'expt_get_running_experiments': [string, ["pointer"]],
        'expt_free_string': [voidType, [string]],
        'expt_last_error_message': [string, []],
        'expt_last_error_length': [int, []],
        'expt_free_client': [voidType, ["pointer"]]
    });

    constructor(tenantName: string, pollingFrequency: number, cacHostName: string) {
        if (!tenantName || tenantName == "") {
            throw Error("tenantName cannot be null/undefined or empty")
        }
        if (!cacHostName || cacHostName == "") {
            throw Error("cacHostName cannot be null/undefined or empty")
        }
        this.tenant = tenantName;
        this.cacHostName = cacHostName;
        this.pollingFrequency = pollingFrequency;
        let respCode = this.rustLib.expt_new_client(
            this.tenant, this.pollingFrequency, this.cacHostName
        );
        if (respCode == 1) {
            let errorMessage = this.getLastErrorMessage();
            throw Error("Some Error Occured while creating new experimentation client " + errorMessage);
        }
    }

    public getLastErrorMessage(): string {
        return this.rustLib.expt_last_error_message() || "";
    }

    public getClient(): ref.Pointer<unknown> {
        return this.rustLib.expt_get_client(this.tenant);
    }

    public getRunningExpriments(): string {
        let clientPtr = this.getClient();
        return this.rustLib.expt_get_running_experiments(clientPtr) || this.getLastErrorMessage();
    }

    freeString(str: string) {
        this.rustLib.expt_free_string(str);
    }

    public async startPollingUpdate() {
        if (isMainThread && worker) {
            worker.postMessage(
                JSON.stringify({ tenant: this.tenant
                    , event : "startPollingUpdate"
                    , pollingFrequency: this.pollingFrequency
                    , cacHostName: this.cacHostName
                    })
            );
            return;
        }
        if (!isMainThread) {
            this.rustLib.expt_start_polling_update(this.tenant)
        }
    }

    public getLastErrorLength(): number {
        return this.rustLib.expt_last_error_length()
    }

    freeClient(clientPtr: ref.Pointer<unknown>) {
        this.rustLib.expt_free_client(clientPtr);
    }

    public getFilteredSatisfiedExperiments(context: Object, filterPrefix: string[] | undefined): string {
        let strContext = JSON.stringify(context);
        let strFilterPrefix = filterPrefix ? ref.allocCString(filterPrefix.join(this.delimeter)) : ref.NULL;
        return this.rustLib.expt_get_filtered_satisfied_experiments(
            this.getClient(), strContext, strFilterPrefix
        ) || this.getLastErrorMessage();
    }

    public getApplicableVariant(context: Object, toss: number): string {
        let strContext = JSON.stringify(context);
        return this.rustLib.expt_get_applicable_variant(
            this.getClient(), strContext, toss
        ) || this.getLastErrorMessage();
    }

    public getSatisfiedExperiments(context: Object, filterPrefix: string[] | undefined): string {
        let strContext = JSON.stringify(context);
        let strFilterPrefix = filterPrefix ? ref.allocCString(filterPrefix.join(this.delimeter)) : ref.NULL;
        return this.rustLib.expt_get_satisfied_experiments(
            this.getClient(), strContext, strFilterPrefix
        ) || this.getLastErrorMessage();
    }
}

export default ExperimentationClient;