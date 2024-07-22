import Foundation

typealias UnknownClientPointer = OpaquePointer
typealias Value = [String: Any]

func createExptClient(tenant: String, frequency: UInt, hostname: String) -> Bool {
    return tenant.withCString { t -> Bool in
        return hostname.withCString { h -> Bool in
            let resp = expt_new_client(t, frequency, h);
            if (resp == 0) {
                return true
            }
            return false
        }
    }
}

func getExptClient(tenant: String) -> UnknownClientPointer? {
    return tenant.withCString { t -> UnknownClientPointer? in
        return expt_get_client(tenant)
    }
}

func exptStartPolling(tenant: String) {
    tenant.withCString { t in
        expt_start_polling_update(t)
    }
}

func parseJson(jsonString: String) -> Value? {
    if let jsonData = jsonString.data(using: .utf8) {
        do {
            return try JSONSerialization.jsonObject(with: jsonData, options: []) as? [String: Any]
        } catch {
            return nil
        }
    }
    return nil
}

func getApplicableVariant(client: UnknownClientPointer, context: String, toss: Int16) -> Value? {
    return context.withCString { c -> Value? in
        let rawData = expt_get_applicable_variant(client, c, toss)
        return rawData.map { String(cString: $0) }.flatMap { parseJson(jsonString: $0) }
    }
}

func getSatisfiedExperiments(client: UnknownClientPointer, context: String, filterPrefix: [String]) -> Value? {
    let keys = filterPrefix.joined(separator: "|")

    return keys.withCString { k -> Value? in
        return context.withCString { c -> Value? in
            let rawData = expt_get_satisfied_experiments(client, c, k)
            return rawData.map { String(cString: $0) }.flatMap { parseJson(jsonString: $0) }
        }
    }
}

func getRunningExperiments(client: UnknownClientPointer) -> Value? {
    let resp = expt_get_running_experiments(client)
    return resp.map { String(cString: $0) }.flatMap { parseJson(jsonString: $0) }
}


func getFilteredSatisfiedExperiments(client: UnknownClientPointer, context: String, filterPrefix: [String]) -> Value? {
    let keys = filterPrefix.joined(separator: "|")
    return context.withCString { c -> Value? in
        return keys.withCString { k -> Value? in
            let resp = expt_get_filtered_satisfied_experiments(client, c, k)
            return resp.map { String(cString: $0) }.flatMap { parseJson(jsonString: $0) }
        }
    }
}

func exptFreeClient(client: UnknownClientPointer) {
    expt_free_client(client)
}

func exptLastErrorMessage() -> String? {
    return expt_last_error_message().map { String(cString: $0) }
}
