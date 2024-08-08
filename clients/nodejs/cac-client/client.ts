import * as ffi from 'ffi-napi';
import * as ref from 'ref-napi';
import * as path from 'path';
import os from 'os';
import { Worker, isMainThread} from "node:worker_threads";
import { parentPort } from 'worker_threads';

let libPathEnc: string | undefined = process.env.SUPERPOSITION_LIB_PATH;

if (libPathEnc == "" || libPathEnc == undefined) {
    throw new Error("SUPERPOSITION_LIB_PATH not found in env");
}

let platform = os.platform();
let fileName =
        platform == "darwin" ?
            'libcac_client.dylib' :
            platform == "linux" ?
                'libcac_client.dll' :
                'libcac_client.so'

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
    let cac_client: Map < String, CacClient > = new Map();
    parentPort.on("message", (message) => {
        if (message) {
            if (message.event === "startCACPollingUpdate") {
                let {
                    tenant,
                    pollingFrequency,
                    cacHostName,
                } = message;
                let tenantClient = cac_client.get(tenant);
                if (tenantClient) {
                    tenantClient.startCACPollingUpdate();
                } else {
                    tenantClient = new CacClient(tenant, pollingFrequency, cacHostName);
                    cac_client.set(tenant, tenantClient);
                    tenantClient.startCACPollingUpdate();
                }
            }
        }
    })
}
// -----------------------------------------

class CacClient {
    tenant: string | null = null;
    cacHostName: string | null = null;
    pollingFrequency: number = 10;

    rustLib = ffi.Library(libPath, {
        'cac_new_client': [int, [string, int, string]],
        'cac_get_client': [string, [string]],
        'cac_start_polling_update': [voidType, [string]],
        'cac_free_client': [voidType, [string]],
        'cac_last_error_message': [string, []],
        'cac_get_config': [string, [string, string, string]],
        'cac_last_error_length': [int, [voidType]],
        'cac_free_string': [voidType, [string]],
        'cac_get_last_modified': [string, [string]],
        'cac_get_resolved_config': [string, [string, string, string, string]],
        'cac_get_default_config': [string, [string, string]],

    });

    constructor(tenantName: string, pollingFrequency: number, cacHostName: string) {
        if (tenantName == "" || cacHostName == "") {
            throw Error("tenantName cannot be null or empty")
        }
        this.tenant = tenantName;
        this.pollingFrequency = pollingFrequency;
        this.cacHostName = cacHostName;
    }

    public getCACLastErrorMessage(): string | null {
        return this.rustLib.cac_last_error_message();
    }

    public getCACLastErrorLength(): number {
        return this.rustLib.cac_last_error_length()
    }

    public getCACClient(): string | null {
        return this.rustLib.cac_get_client(this.tenant);
    }

    public createNewCACClient(): number {
        let resp = this.rustLib.cac_new_client(this.tenant, this.pollingFrequency, this.cacHostName);
        if (resp == 1) {
            let errorMessage = this.getCACLastErrorMessage();
            console.error("Some Error Occur while creating new client ", errorMessage);
        }
        return resp;
    }

    public async startCACPollingUpdate() {
        if (isMainThread && worker) {
            worker.postMessage(
                { tenant: this.tenant
                , event : "startCACPollingUpdate"
                , pollingFrequency: this.pollingFrequency
                , cacHostName: this.cacHostName
                }
            );
            return;
        }
        if (!isMainThread) {
            this.rustLib.cac_start_polling_update(this.tenant)
        }
    }

    public getCACConfig(filterQuery: string, filterPrefix: string): string | null {
        let clientPtr = this.getCACClient();
        return this.rustLib.cac_get_config(clientPtr, filterQuery, filterPrefix);
    }

    public freeCACClient(clientPtr: string) {
        this.rustLib.cac_free_client(clientPtr)
    }

    public freeCACString(str: string) {
        this.rustLib.cac_free_string(str)
    }

    public getLastModified(): string | null {
        return this.rustLib.cac_get_last_modified(this.getCACClient());
    }

    public getResolvedConfig(query: string, filterKeys: string, mergeStrategy: string): string | null {
        return this.rustLib.cac_get_resolved_config(
            this.getCACClient(), query, filterKeys, mergeStrategy
        )
    }
    public getDefaultConfig(filterKeys: string): string | null {
        return this.rustLib.cac_get_default_config(
            this.getCACClient(), filterKeys
        )
    }
}

export default CacClient;