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
        try {
            message = JSON.parse(message);
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
                    tenantClient = new CacClient(tenant, pollingFrequency, cacHostName);
                    cac_client.set(tenant, tenantClient);
                    tenantClient.startPollingUpdate();
                }
            }
        } catch (error) {
            console.log("Error While starting polling Update for cac client ", error);
        }
    })
}
// -----------------------------------------

export enum MergeStrategy {
    MERGE = "MERGE",
    REPLACE = "REPLACE"
}

export class CacClient {
    tenant: string | null = null;
    cacHostName: string | null = null;
    pollingFrequency: number = 10;
    delimeter: string = ",";

    rustLib = ffi.Library(libPath, {
        'cac_new_client': [int, [string, int, string]],
        'cac_get_client': ["pointer", [string]],
        'cac_start_polling_update': [voidType, [string]],
        'cac_free_client': [voidType, ["pointer"]],
        'cac_last_error_message': [string, []],
        'cac_get_config': [string, ["pointer", "pointer", "pointer"]],
        'cac_last_error_length': [int, [voidType]],
        'cac_free_string': [voidType, ["pointer"]],
        'cac_get_last_modified': [string, ["pointer"]],
        'cac_get_resolved_config': [string, ["pointer", string, "pointer", string]],
        'cac_get_default_config': [string, ["pointer", "pointer"]],

    });

    constructor(tenantName: string, pollingFrequency: number, cacHostName: string) {
        if (!tenantName || tenantName == "") {
            throw Error("tenantName cannot be null or empty")
        }
        if (!cacHostName || cacHostName == "") {
            throw Error("cacHostName cannot be null or empty")
        }
        this.tenant = tenantName;
        this.pollingFrequency = pollingFrequency;
        this.cacHostName = cacHostName;
        let resp = this.rustLib.cac_new_client(this.tenant, this.pollingFrequency, this.cacHostName);
        if (resp == 1) {
            let errorMessage = this.getLastErrorMessage();
            throw Error("Some Error Occur while creating new client " + errorMessage);
        }
    }

    public getLastErrorMessage(): string {
        return this.rustLib.cac_last_error_message() || "";
    }

    public getLastErrorLength(): number {
        return this.rustLib.cac_last_error_length()
    }

    public getClient(): ref.Pointer<unknown> {
        return this.rustLib.cac_get_client(this.tenant);
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
            this.rustLib.cac_start_polling_update(this.tenant)
        }
    }

    public getConfig(filterQuery: Object | undefined, filterPrefix: string[] | undefined): string {
        let strFilterQuery = filterQuery ? ref.allocCString(JSON.stringify(filterQuery)) : ref.NULL;
        let strFilterPrefix = filterPrefix ? ref.allocCString(filterPrefix.join(this.delimeter)) : ref.NULL;
        let clientPtr = this.getClient();
        let resp = this.rustLib.cac_get_config(clientPtr, strFilterQuery, strFilterPrefix) || this.getLastErrorMessage();
        return resp;
    }

    freeClient(clientPtr: ref.Pointer<unknown>) {
        this.rustLib.cac_free_client(clientPtr);
    }

    freeString(str: ref.Pointer<unknown>) {
        this.rustLib.cac_free_string(str);
    }

    public getLastModified(): string {
        return this.rustLib.cac_get_last_modified(this.getClient()) || this.getLastErrorMessage();
    }

    public getResolvedConfig(query: Object, filterKeys: string[] | undefined, mergeStrategy: MergeStrategy): string {
        let strQuery = JSON.stringify(query);
        let strFilterKeys = filterKeys ? ref.allocCString (filterKeys.join("|")) : ref.NULL ;
        let resp = this.rustLib.cac_get_resolved_config(
            this.getClient(), strQuery, strFilterKeys, mergeStrategy
        ) || this.getLastErrorMessage();
        return resp;
    }
    public getDefaultConfig(filterKeys: string[] | undefined): string {
        let strFilterKeys = filterKeys ? ref.allocCString(filterKeys.join("|")) : ref.NULL;
        let resp = this.rustLib.cac_get_default_config(
            this.getClient(), strFilterKeys
        ) || this.getLastErrorMessage();
        return resp;
    }
}