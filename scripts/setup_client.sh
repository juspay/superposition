#!/bin/bash

download_binary() {
  local url=$1
  local zip_name=$2

  if [ -z "$url" ]; then
    echo "Unsupported platform/architecture combination"
    return 1
  fi

  if ! curl "$url" -L -o "$zip_name"; then
    echo "Failed to download binary: $?"
    return 1
  fi

  unzip "$zip_name" -d "superposition"
  rm "$zip_name"
}


# Define the URL for each platform
declare -A cac_urls
cac_urls["windows-x86_64"]="https://github.com/Datron/superposition/releases/download/testing/superposition_cac_client-x86_64-pc-windows-msvc.zip"
cac_urls["linux-x86_64"]="https://github.com/Datron/superposition/releases/download/testing/superposition_cac_client-x86_64-unknown-linux-gnu.zip"
cac_urls["darwin-x86_64"]="https://github.com/Datron/superposition/releases/download/testing/superposition_cac_client-x86_64-apple-darwin.zip"
cac_urls["darwin-aarch64"]="https://github.com/Datron/superposition/releases/download/testing/superposition_cac_client-aarch64-apple-darwin.zip"

declare -A exp_urls
exp_urls["windows-x86_64"]="https://github.com/Datron/superposition/releases/download/testing/superposition_experimentation_client-x86_64-pc-windows-msvc.zip"
exp_urls["linux-x86_64"]="https://github.com/Datron/superposition/releases/download/testing/superposition_experimentation_client-x86_64-unknown-linux-gnu.zip"
exp_urls["darwin-x86_64"]="https://github.com/Datron/superposition/releases/download/testing/superposition_experimentation_client-x86_64-apple-darwin.zip"
exp_urls["darwin-aarch64"]="https://github.com/Datron/superposition/releases/download/testing/superposition_experimentation_client-aarch64-apple-darwin.zip"

# Determine the current platform
platform=$(uname -s)
arch=$(uname -m )

# Map the platform and arch to the correct URL
case $platform in
  MINGW|Win32|MSYS|CYGWIN*)
    platform="windows"
    ;;
  Linux)
    platform="linux"
    ;;
  Darwin)
    platform="darwin"
    ;;
  *)
    echo "Unsupported platform: $platform"
    exit 1
    ;;
esac

case $arch in
  x86_64)
    arch="x86_64"
    ;;
  arm64)
    arch="aarch64"
    ;;
  *)
    echo "Unsupported architecture: $arch"
    exit 1
    ;;
esac

# Download the binary
download_binary "${cac_urls[$platform-$arch]}" "superposition_cac_client.zip"
download_binary "${exp_urls[$platform-$arch]}" "superposition_exp_client.zip"

mv superposition/headers/* superposition/
mv superposition/target/**/release/* superposition/
rm -rf superposition/headers superposition/target
