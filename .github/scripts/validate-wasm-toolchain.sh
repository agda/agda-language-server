#!/usr/bin/env bash
# validate-wasm-toolchain.sh
#
# Part of the .ghc-wasm toolchain producer (ci.md PR 1 commit 3): a
# ghc-wasm-meta setup only counts as a successful producer once the
# resulting toolchain executables actually work, not merely because
# setup.sh exited zero.
set -euo pipefail

wasm32-wasi-ghc --version
wasm32-wasi-cabal --version
