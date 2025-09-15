# How to build for WASM

1. Setup a working ghc (tested with v9.10) with WASM backend and wasm32-wasi-cabal.
2. `cd` into `wasm-submodules/network` and run `autoreconf -i`.
3. In the project root, run `cp cabal.project{.wasm32,}`, and then `wasm32-wasi-cabal build`.

Note: This project uses a hybrid approach - most dependencies use cabal's git handling, but the network package remains as a git submodule due to autotools requirements.

The process might output the following:

```
[442 of 452] Compiling Language.LSP.Protocol.Message.Types ( src/Language/LSP/Protocol/Message/Types.hs, /home/qbane/agda-project/haskell-lsp-wasm/dist-newstyle/build/wasm32-wasi/ghc-9.10.1.20250207/lsp-types-2.3.0.1/build/Language/LSP/Protocol/Message/Types.o, /home/qbane/agda-project/haskell-lsp-wasm/dist-newstyle/build/wasm32-wasi/ghc-9.10.1.20250207/lsp-types-2.3.0.1/build/Language/LSP/Protocol/Message/Types.dyn_o )

wasm://wasm/001e3c92:1


RuntimeError: table index is out of bounds
    at wasm://wasm/001e3c92:wasm-function[586]:0x45e40
    at wasm://wasm/001e3c92:wasm-function[365]:0x286e1
    at wasm://wasm/001e3c92:wasm-function[595]:0x46135
    at process.processImmediate (node:internal/timers:491:21)

Node.js v22.14.0
```

At this moment, you should terminate the process and run it again.

This was a known issue ([ghc#26106](https://gitlab.haskell.org/ghc/ghc/-/issues/26106)) and was fixed in the [GHC upstream](https://gitlab.haskell.org/ghc/ghc/-/commit/3e4a456801ba6aab5d8a6c0ae5a4ba8ab99b9d25).

If everything works properly, it should build a binary `als.wasm`.
