# How to build for WASM

1. Setup a working ghc (tested with v9.10) with WASM backend and wasm32-wasi-cabal.
2. `cd` into `wasm-submodules/network` and run `autoreconf -i`.
3. In the project root, run `cp cabal.project{.wasm32,}`, and then `wasm32-wasi-cabal build`.

Note: This project uses a hybrid approach - most dependencies use cabal's git handling, but the network package remains as a git submodule due to autotools requirements.
