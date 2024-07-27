## CAC and Experimentation clients in `Swift`

- ## Walkthrough
    - Create bridging file for each module
    - add the `#include` statement for the C header file
    - `using nix`: use `swiftc` to compile modules using `-L`, `-l` and `-import-objc-header` flags to specify object files search dir, object-module name and bridging file path respectively. (checkout : [default.nix](default.nix))
    - `using xcode`: create a command-line project in xcode and configure `-L`, `-l` and `-import-objc-header` flags using `Build Settings` in the GUI. Settings equivalent for each compiler flags (checkout : [project.pbxproj](swift.xcodeproj/project.pbxproj))

        - `-L` : `Library Search Paths`
        - `-l` : `Other Linker Flags`
        - `-import-objc-header` : `Objective-C Bridging Header`

- ## setup
    - ### using nix :
        1. cd to `clients/swift`
        2. spawn devShell `nix develop .#swift`
        3. run `compileTest`
        4. use generated bins

    - ### using xcode :
        1. open project (`clients/swift`) in xcode
        2. build & run the project
