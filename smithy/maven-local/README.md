# smithy/maven-local

Bundled local Maven repository for `smithy-mcp-codegen` and `smithy-mcp-traits`.

## Why this is here

The smithy-mcp-generator does not yet publish its artifacts to a public Maven repository (and GitHub Packages requires authentication for Maven, even for public repos). To unblock superposition's MCP server work without setting up new publishing infrastructure, the generator JARs are bundled here in standard Maven layout. `SMITHY_MAVEN_REPOS` in the makefile points at this directory via a `file://` URL.

## How to refresh

When the generator is updated and a new version needs to be vendored:

```bash
cd ../smithy-mcp-generator
git checkout <tag-or-sha>
./gradlew :smithy-mcp-traits:publishToMavenLocal :smithy-mcp-codegen:publishToMavenLocal

cd ../superposition
rm -rf smithy/maven-local/in/juspay/smithy/{smithy-mcp-codegen,smithy-mcp-traits}
cp -r ~/.m2/repository/in/juspay/smithy/smithy-mcp-codegen smithy/maven-local/in/juspay/smithy/
cp -r ~/.m2/repository/in/juspay/smithy/smithy-mcp-traits  smithy/maven-local/in/juspay/smithy/
```

Bump `runtimeVersion` in `smithy/smithy-build.json` to match.

## Migration plan

This is transitional. See spec §11.4 — eventual home is either the juspay sandbox Maven repo (already in `SMITHY_MAVEN_REPOS`) or JitPack. When that lands, delete this directory and remove the `file://` entry from `SMITHY_MAVEN_REPOS`.
