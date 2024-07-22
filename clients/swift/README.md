## CAC and Experimentation clients in `Swift`

- ## Walkthrough
    - Create bridging file for each module
    - add the `#include` statement for the C header file
    - without xcode: use `swiftc` to compile modules using `-L`, `-l` and `-import-objc-header` flags to specify object files search dir, object-module name and bridging file path respectively. (checkout : [default.nix](default.nix))
    - with xcode: todo!

- ## setup (nix) :
    `nix develop .#swift`

- ## run without xcode (using nix) :
    1. to compile cac client: `compileCac`
    2. to compile exp client: `compileExp`
    3. use generated bins

- ## run using xcode :
    todo!

- ## run example :
    todo!
