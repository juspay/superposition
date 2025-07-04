import os
import sys
import platform
import urllib.request
import zipfile
from setuptools import setup, find_packages
from setuptools.command.build_py import build_py as build_py_orig

# === Determine the correct binary name ===
def get_library_filename():
    base = "libsuperposition_core"
    if sys.platform.startswith("darwin"):
        return f"{base}.dylib"
    elif sys.platform.startswith("win"):
        return f"{base}.dll"
    else:
        return f"{base}.so"

# === Get platform-specific download URL ===
def get_platform_zip_url():
    base_url = "https://github.com/juspay/superposition/releases/download/v0.76.0"
    triple_map = {
        ("darwin", "arm64"): "aarch64-apple-darwin",
        ("darwin", "x86_64"): "x86_64-apple-darwin",
        ("linux", "x86_64"): "x86_64-unknown-linux-gnu",
        # Add Windows or other triples if needed
    }

    triple = triple_map.get((sys.platform, platform.machine()))
    if not triple:
        raise RuntimeError(f"❌ Unsupported platform: {sys.platform} / {platform.machine()}")

    return f"{base_url}/superposition_core-{triple}.zip"

# === Custom build command to download binary if needed ===
class build_py(build_py_orig):
    def run(self):
        lib_name = get_library_filename()
        lib_path = os.path.join("superposition_bindings", lib_name)

        if not os.path.exists(lib_path):
            print(f"🔽 Downloading platform-specific binary for {sys.platform} {platform.machine()}...")
            zip_url = get_platform_zip_url()
            zip_file = "temp_lib.zip"
            urllib.request.urlretrieve(zip_url, zip_file)

            with zipfile.ZipFile(zip_file, "r") as zip_ref:
                found = False
                for name in zip_ref.namelist():
                    if name.endswith(lib_name):
                        zip_ref.extract(name, "superposition_bindings")
                        extracted_path = os.path.join("superposition_bindings", name)
                        if name != lib_name:
                            os.rename(extracted_path, lib_path)
                        found = True
                        break

                if not found:
                    raise FileNotFoundError(f"{lib_name} not found in archive.")

            os.remove(zip_file)
            print(f"✅ Binary extracted to: {lib_path}")

        super().run()

# === Setup config ===
LIBRARY_FILENAME = get_library_filename()

setup(
    name="superposition_bindings",
    version="0.1.0",
    packages=find_packages(),
    package_data={"superposition_bindings": ["libsuperposition_core.*"]},
    include_package_data=True,
    cmdclass={"build_py": build_py},
    python_requires=">=3.8",
)