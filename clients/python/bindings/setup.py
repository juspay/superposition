import os
import sys
import platform
from setuptools import setup, find_packages
from setuptools.command.build_py import build_py as build_py_orig


# === get the right binary name ===
def get_library_filename():
    triple_map = {
        ("darwin", "arm64"): "aarch64-apple-darwin.dylib",
        ("darwin", "x86_64"): "x86_64-apple-darwin.dylib",
        ("linux", "x86_64"): "x86_64-unknown-linux-gnu.so",
        ("win32", "x86_64"): "x86_64-pc-windows-msvc.dll",
    }

    triple = triple_map.get((sys.platform, platform.machine()))
    if not triple:
        raise RuntimeError(f"❌ Unsupported platform: {sys.platform} / {platform.machine()}")

    return f"libsuperposition_core-{triple}"

# === Custom build command to download binary if needed ===
class build_py(build_py_orig):
    def run(self):
        lib_name = get_library_filename()
        lib_path = os.path.join("superposition_bindings", lib_name)

        if not os.path.exists(lib_path):
            raise FileNotFoundError(f"{lib_name} not found in folder.")
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
