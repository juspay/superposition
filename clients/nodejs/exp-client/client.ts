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
        if (message) {
            if (message.event === "startExperimantionPollingUpdate") {
                let {
                    tenant,
                    pollingFrequency,
                    cacHostName,
                } = message;
                let tenantClient = cac_client.get(tenant);
                if (tenantClient) {
                    tenantClient.startExperimantionPollingUpdate();
                } else {
                    tenantClient = new ExperimentationClient(tenant, pollingFrequency, cacHostName);
                    cac_client.set(tenant, tenantClient);
                    tenantClient.startExperimantionPollingUpdate();
                }
            }
        }
    })
}
// -----------------------------------------

class ExperimentationClient {
    tenant: string | null = null;
    pollingFrequency: number = 10;
    cacHostName: string | null = null;

    rustLib = ffi.Library(libPath, {
        'expt_new_client': [int, [string, int, string]],
        'expt_start_polling_update': [voidType, [string]],
        'expt_get_client': [string, [string]],
        'expt_get_applicable_variant': [string, [string, string, int]],
        'expt_get_satisfied_experiments': [string, [string, string, string]],
        'expt_get_filtered_satisfied_experiments': [string, [string, string, string]],
        'expt_get_running_experiments': [string, [string]],
        'expt_free_string': [voidType, [string]],
        'expt_last_error_message': [string, []],
        'expt_last_error_length': [int, []],
        'expt_free_client': [voidType, [string]]
    });

    constructor(tenantName: string, pollingFrequency: number, hostName: string) {
        this.tenant = tenantName;
        this.cacHostName = hostName;
        this.pollingFrequency = pollingFrequency;
    }

    public getExperimentationLastErrorMessage(): string | null {
        return this.rustLib.expt_last_error_message();
    }

    public createNewExperimentaionClient(): number {
        let respCode = this.rustLib.expt_new_client(
            this.tenant, this.pollingFrequency, this.cacHostName
        );
        if (respCode == 1) {
            let errorMessage = this.getExperimentationLastErrorMessage();
            console.log("Some Error Occured while creating new experimentation client ", errorMessage);
            throw Error("Client Creation Error");
        }
        return respCode;
    }

    public getExperimentationClient(): string | null {
        return this.rustLib.expt_get_client(this.tenant);
    }

    public getRunningExpriments(): string | null {
        let clientPtr = this.getExperimentationClient();
        return this.rustLib.expt_get_running_experiments(clientPtr);
    }

    public freeString(str: string) {
        this.rustLib.expt_free_string(str);
    }

    public async startExperimantionPollingUpdate() {
        if (isMainThread && worker) {
            worker.postMessage(
                { tenant: this.tenant
                , event : "startExperimantionPollingUpdate"
                , pollingFrequency: this.pollingFrequency
                , cacHostName: this.cacHostName
                }
            );
            return;
        }
        if (!isMainThread) {
            this.rustLib.expt_start_polling_update(this.tenant)
        }
    }

    public getExperimentationLastErrorLength(): number {
        return this.rustLib.expt_last_error_length()
    }

    public freeExprementationClient() {
        this.rustLib.expt_free_client(this.getExperimentationClient());
    }

    public getFilteredSatisfiedExperiments(context: string, filterPrefix: string): string | null {
        return this.rustLib.expt_get_filtered_satisfied_experiments(
            this.getExperimentationClient(), context, filterPrefix
        );
    }

    public getApplicableVariant(context: string, toss: number): string | null {
        return this.rustLib.expt_get_applicable_variant(
            this.getExperimentationClient(), context, toss
        );
    }

    public getSatisfiedExperiments(context: string, filterPrefix: string): string | null {
        return this.rustLib.expt_get_satisfied_experiments(
            this.getExperimentationClient(), context, filterPrefix
        );
    }
}

export default ExperimentationClient;