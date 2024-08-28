//
//  utils.swift
//  swift
//
//  Created by naman agarwal on 27/07/24.
//

import Foundation

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
