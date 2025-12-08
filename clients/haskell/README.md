# Haskell Openfeature provider

This directory contains a Haskell implementation of an OpenFeature provider using Superposition. It requires the binary superposition_core, which is available to download from github releases or can be built from source. This tutorial assumes you have setup superposition locally or have access to a running superposition instance.

## Setup (Nix):

Create a flake.nix file with the following code:
```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    superposition = {
      url = "github:juspay/superposition";
      inputs = {
        haskell-flake.follows = "haskell-flake";
        flake-parts.follows = "flake-parts";
        # superposition doesn't work with latest nixpkgs yet
        # nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem =
        {
          config,
          pkgs,
          system,
          ...
        }:
        {
          haskellProjects.default = {
            imports = [
              inputs.superposition.haskellFlakeProjectModules.output
            ];

            basePackages = pkgs.haskell.packages.ghc96;

            settings = {
              monad-logger-aeson = {
                broken = false;
              };
            };
            autoWire = [
              "packages"
              "apps"
              "checks"
            ]; # Wire all but the devShell
          };

          devShells.default = pkgs.mkShell {
            name = "superposition openfeature dev shell";
            inputsFrom = [
              pkgs.haskell.compiler.ghc94 # pick your preferred ghc version
              config.haskellProjects.default.outputs.devShell
              inputs.superposition.devShells.${system}.haskell
            ];
          };
        };
    };
}
```

Create a cabal.project file with the following code:

```cabal
packages: .
source-repository-package
  type: git
  location: https://github.com/juspay/open-feature-haskell-sdk.git
source-repository-package
  type: git
  location: https://github.com/juspay/superposition.git
  tag: {version-you-want-to-use} # e.g., v0.95.1
  subdir: clients/haskell/superposition-bindings
          clients/haskell/sdk
          clients/haskell/open-feature-provider
```

Add the following dependencies to your .cabal file:

```cabal
  monad-logger-aeson == 0.4.1.3,
  open-feature,
  superposition-open-feature-provider,
```

When the flake builds, you can start using the provider in your Haskell applications

## Setup (Without Nix):

Use the following bash script to fetch superposition_core binaries needed for your application, if you don't want to build from source:

```bash
cd /tmp

curl -L "https://github.com/juspay/superposition/releases/download/{version-you-want-to-use}/superposition_core-${1}.zip" \
     -o libsuperposition.zip

unzip -od libsuperposition libsuperposition.zip

cp libsuperposition/core.h /usr/local/include/superposition_core.h
cp libsuperposition/libsuperposition_core.* /usr/local/lib/
if [ "$(uname)" = "Darwin" ]; then
    install_name_tool -id /usr/local/lib/libsuperposition_core.dylib /usr/local/lib/libsuperposition_core.dylib
fi
```

You can run this script with the architecture as an argument, e.g.,

```bash
./fetch_superposition_core.sh 'aarch64-apple-darwin'
```
Supported architectures are:
- x86_64-unknown-linux-gnu
- x86_64-pc-windows-msvc
- x86_64-apple-darwin
- aarch64-apple-darwin

Create a cabal.project file with the following code:

```cabal
packages: .
source-repository-package
  type: git
  location: https://github.com/juspay/open-feature-haskell-sdk.git
source-repository-package
  type: git
  location: https://github.com/juspay/superposition.git
  tag: {version-you-want-to-use} # e.g., v0.95.1
  subdir: clients/haskell/superposition-bindings
          clients/haskell/sdk
          clients/haskell/open-feature-provider
```

Add the following dependencies to your .cabal file:

```cabal
  monad-logger-aeson == 0.4.1.3,
  open-feature,
  superposition-open-feature-provider,
```

After setting up the dependencies, you can start using the provider in your Haskell applications.

## Usage

Here is a simple example of how to use the Superposition OpenFeature provider in a Haskell application:

```haskell
module Main (main) where

import qualified Data.OpenFeature.Api as OpenFeature
import qualified Data.OpenFeature.Client as OpenFeature
import qualified Data.OpenFeature.FeatureProvider as OpenFeature
import qualified Data.OpenFeature.EvaluationContext as OpenFeature
import qualified Data.OpenFeature.SuperpositionProvider as Superposition
import Data.Text
import GHC.Conc.IO (threadDelay)
import qualified Network.URI as URI
import qualified Data.Aeson as Aeson

expectJust :: Maybe a -> a
expectJust (Just a) = a
expectJust _ = undefined

expectRight :: Either b a -> a
expectRight (Right a) = a
expectRight (Left err) = undefined

main :: IO ()
main = do
  let options =
        Superposition.defaultProviderOptions
          { Superposition.orgId = pack "orgid162145664241766405",
            Superposition.workspaceId = pack "test1",
            Superposition.endpoint = expectJust $ URI.parseURI "http://localhost:8080",
            Superposition.refreshOptions = Superposition.Poll 10,
            Superposition.logLevel = Superposition.LevelDebug
          }
  provider <- expectRight <$> Superposition.newSuperpositionProvider options
  !_ <- OpenFeature.initialize provider OpenFeature.defaultContext
  -- wait a few seconds...
  threadDelay 4000000
  OpenFeature.setDefaultProvider provider
  client <- OpenFeature.createClient
  v <- expectRight <$> OpenFeature.getBoolValue client (pack "k1") mempty
  putStrLn $ show v
  let context = OpenFeature.withCustomField "name" (Aeson.toJSON ("john"::String)) (OpenFeature.defaultContext)
  v1 <- expectRight <$> OpenFeature.getBoolValue client (pack "k1") (Just context)
  putStrLn $ show v1
  pure ()
```

If everything is setup right, then you should be able to compile and run this code.