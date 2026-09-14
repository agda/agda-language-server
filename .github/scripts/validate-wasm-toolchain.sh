#!/usr/bin/env bash
# validate-wasm-toolchain.sh
#
# Part of the .ghc-wasm toolchain producer: a ghc-wasm-meta setup only
# counts as a successful producer once the resulting toolchain executables
# actually work, not merely because setup.sh exited zero.
set -euo pipefail

wasm32-wasi-ghc --version
wasm32-wasi-cabal --version
