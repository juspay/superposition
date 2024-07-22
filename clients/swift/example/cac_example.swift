import Foundation

func main() {
    let t = "test"
    let f : UInt = 300
    let h = "http://localhost:8080"

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

            print("Last Modified: \(m)")
            print("Resolved Config: \(r)")
            print("Default Config: \(d)")

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

main()
