## CAC and Experimentation clients in `Swift`

- ## Walkthrough
    - Create bridging file for each module
    - add the `#include` statement for the C header file
    - `using nix`: use `swiftc` to compile modules using `-L`, `-l`, `I` and `-import-objc-header` flags to specify object files search dir, object-module name, header search path and bridging file path respectively. (checkout : [default.nix](default.nix))
    - `using xcode`: create a command-line project in xcode and configure `-L`, `-l` and `-import-objc-header` flags using `Build Settings` in the GUI. Settings equivalent for each compiler flags (checkout : [Config](Config.xcconfig))

        - `-L` : `LIBRARY_SEARCH_PATHS`
        - `-l` : `OTHER_LDFLAGS`
        - `-import-objc-header` : `SWIFT_OBJC_BRIDGING_HEADER`
        - `I` : `HEADER_SEARCH_PATHS`

- ## setup
    - ### using nix :
        1. cd to `clients/swift`
        2. spawn devShell `nix develop .#swift`
        3. run `compileTest`
        4. use generated bins

    - ### using xcode :
        1. open project (`clients/swift`) in xcode
        2. build & run the project
