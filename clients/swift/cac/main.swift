import Foundation

typealias UnknownClientPointer = OpaquePointer
enum MergeStrategy {
    case MERGE
    case REPLACE

    var show: String {
        switch self {
        case .MERGE:
            return "MERGE"
        case .REPLACE:
            return "REPLACE"
        }
    }
}

func createCacClient(tenant: String, frequency: UInt, hostname: String) -> Bool {
    return tenant.withCString { t -> Bool in
        return hostname.withCString { h -> Bool in
            let resp = cac_new_client(t, frequency, h);
            if (resp == 0) {
                return true
            }
            return false
        }
    }
}

func getCacClient(tenant: String) -> UnknownClientPointer? {
    return tenant.withCString { t -> UnknownClientPointer? in
        return cac_get_client(tenant)
    }
}

func cacStartPolling(tenant: String) {
    tenant.withCString { t in
        cac_start_polling_update(t)
    }
}

func getCacLastModified(client: UnknownClientPointer) -> String? {
    let resp = cac_get_last_modified(client)
    return resp.map { String(cString: $0) }
}

func parseJson(jsonString: String) -> Any? {
    if let jsonData = jsonString.data(using: .utf8) {
        do {
            return try JSONSerialization.jsonObject(with: jsonData, options: [])
        } catch {
            return nil
        }
    }
    return nil
}

// TODO: fix
func getResolvedConfig(client: UnknownClientPointer, context: String, filterKeys: [String]? = nil) -> Any? {
    let keys = filterKeys.map { $0.joined(separator: "|") }

    return context.withCString { c -> Any? in
        return MergeStrategy.MERGE.show.withCString { m -> Any? in
            let rawData : UnsafePointer<CChar>?
            if let k = keys {
                rawData = cac_get_resolved_config(client, c, k, m)
            } else {
                rawData = cac_get_resolved_config(client, c, nil, m)
            }
            return rawData.map { String(cString: $0) }.flatMap { parseJson(jsonString: $0) }
        }
    }
}

func getDefaultConfig(client: UnknownClientPointer, filterKeys: [String]) -> Any? {
    let keys = filterKeys.joined(separator: "|")

    return keys.withCString { k -> Any? in
        let rawData = cac_get_default_config(client, keys)
        return rawData.map { String(cString: $0) }.flatMap { parseJson(jsonString: $0) }
    }
}

func cacFreeClient(client: UnknownClientPointer) {
    cac_free_client(client)
}

func cacLastErrorMessage() -> String? {
    return cac_last_error_message().map { String(cString: $0) }
}


let t = "test"
let f : UInt = 300
let h = "http://localhost:8080"

if (createCacClient(tenant: t, frequency: f, hostname: h)) {
    if let client = getCacClient(tenant: t) {
        // cacStartPolling(tenant: t)
        // print("cacStartPolling started!")

        // let m = getCacLastModified(client: client)
        let r = getResolvedConfig(client: client, context: "")
        // let d = getDefaultConfig(client: client, filterKeys: [])

        if let val = r {
            print("resolved: \(r)")
        } else {
            print("err-msg: \(cacLastErrorMessage())")
        }

        // print("modified: \(m)")

        // print("default: \(d)")

        cacFreeClient(client: client)
    } else {
        print("getCacClient failed!")
    }
} else {
    print("createCacClient failed!")
}

// func getFullConfigStateWithFilter(client: UnknownClientPointer, )

// let result = createCacClient(tenant: "naman", frequency: 3, hostname: "http://localhost:8080")
// let result = getCacClient(tenant: "naman")
// let result = cacStartPolling(tenant: "naman")
// let r4 = getCacLastModified(client: "naman")

// print("Result from C function: \(r4)")
