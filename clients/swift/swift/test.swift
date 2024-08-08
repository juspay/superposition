//
//  test.swift
//  swift
//
//  Created by naman agarwal on 27/07/24.
//

import Foundation

// test CAC functions
func test_cac() {
    let t = "dev"
    let f : UInt = 300
    let h = "http://localhost:8080"
    
    print("\n")
    if (createCacClient(tenant: t, frequency: f, hostname: h)) {
        print("createCacClient success!")

        if let client = getCacClient(tenant: t) {
            print("getCacClient success!")

            let thread = Thread {
                cacStartPolling(tenant: t)
            }
            thread.start()
            print("cacStartPolling: thread started!")

            let m = getCacLastModified(client: client)
            let r = getResolvedConfig(client: client, context: "{}")
            let d = getDefaultConfig(client: client, filterKeys: [])
            let c = getConfig(client: client, filterQuery: nil, filterPrefix: nil)
            
            print("Last Modified: \(m)")
            print("Resolved Config: \(r)")
            print("Default Config: \(d)")
            print("Get Config: \(c)")
            
            thread.cancel()
            print("cacStartPolling: thread closed!")

            cacFreeClient(client: client)
            print("Free client memory!")
        } else {
            print("getCacClient failed!")
        }
    } else {
        print("createCacClient failed!")
    }
}

// test EXP functions
func test_exp() {
    let t = "dev"
    let f : UInt = 300
    let h = "http://localhost:8080"
    
    print("\n")
    if (createExptClient(tenant: t, frequency: f, hostname: h)) {
        print("createExptClient successs!")
        if let client = getExptClient(tenant: t) {
            print("getExptClient successs!")

            let thread = Thread {
                exptStartPolling(tenant: t)
            }
            thread.start()
            print("cacStartPolling: thread started!")

            let m = getApplicableVariant(client: client, context: "{}", toss: 1)
            let r = getSatisfiedExperiments(client: client, context: "{}", filterPrefix: [])
            let d = getRunningExperiments(client: client)
            let v = getFilteredSatisfiedExperiments(client: client, context: "{}", filterPrefix: [])
            print("Applicable Variants: \(r)")
            print("Satisfied Experiments: \(m)")
            print("Running Experiments: \(d)")
            print("Filtered Satisfied Experiments: \(v)")

            thread.cancel()
            print("cacStartPolling: thread closed!")

            exptFreeClient(client: client)
            print("Free client memory!")
        } else {
            print("getExptClient failed!")
        }
    } else {
        print("createExptClient failed!")
    }
}
