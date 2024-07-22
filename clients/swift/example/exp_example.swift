import Foundation

func main() {
    let t = "test"
    let f : UInt = 300
    let h = "http://localhost:8080"

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
