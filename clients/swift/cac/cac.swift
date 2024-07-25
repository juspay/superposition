import Foundation

typealias UnknownClientPointer = OpaquePointer
typealias Value = [String: Any]

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
        return cac_get_client(t)
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

func parseJson(jsonString: String) -> Value? {
    if let jsonData = jsonString.data(using: .utf8) {
        do {
            return try JSONSerialization.jsonObject(with: jsonData, options: []) as? Value
        } catch {
            return nil
        }
    }
    return nil
}

func getResolvedConfig(client: UnknownClientPointer, context: String, filterKeys: [String]? = nil) -> Value? {
    let keys = filterKeys.map { $0.joined(separator: "|") }

    return context.withCString { c -> Value? in
        return MergeStrategy.MERGE.show.withCString { m -> Value? in
            let rawData : UnsafePointer<CChar>?
            if let k = keys {
                rawData = k.withCString { ck -> UnsafePointer<CChar>? in
                    return cac_get_resolved_config(client, c, ck, m)
                }
            } else {
                rawData = cac_get_resolved_config(client, c, nil, m)
            }
            return rawData.map { String(cString: $0) }.flatMap { parseJson(jsonString: $0) }
        }
    }
}

func getDefaultConfig(client: UnknownClientPointer, filterKeys: [String]? = nil) -> Value? {
    let keys = filterKeys.map { $0.joined(separator: "|") }
    let rawData : UnsafePointer<CChar>?
    if let k = keys {
        rawData = k.withCString { ck -> UnsafePointer<CChar>? in
            return cac_get_default_config(client, ck)
        }
    } else {
        rawData = cac_get_default_config(client, nil)
    }
    return rawData.map { String(cString: $0) }.flatMap { parseJson(jsonString: $0) }
}

func getConfig(client: UnknownClientPointer, filterQuery: [String]?, filterPrefix: [String]?) -> Value? {
    let query = filterQuery.map { $0.joined(separator: "|") }
    let prefix = filterPrefix.map { $0.joined(separator: "|") }
    let rawData : UnsafePointer<CChar>?

    if let q = query {
        rawData =
            q.withCString { qs -> UnsafePointer<CChar>? in
                if let p = prefix {
                    return p.withCString { ps -> UnsafePointer<CChar>? in
                        return cac_get_config(client, qs, ps)
                    }
                } else {
                    return cac_get_config(client, qs, nil)
                }
            }
    } else {
        if let p = prefix {
            rawData = p.withCString { ps -> UnsafePointer<CChar>? in
                return cac_get_config(client, nil, ps)
            }
        } else {
            rawData = cac_get_config(client, nil, nil)
        }
    }
    return rawData.map { String(cString: $0) }.flatMap { parseJson(jsonString: $0) }
}

func cacFreeClient(client: UnknownClientPointer) {
    cac_free_client(client)
}

func cacLastErrorMessage() -> String? {
    return cac_last_error_message().map { String(cString: $0) }
}
