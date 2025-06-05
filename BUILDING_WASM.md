# How to build on WASM

1. Setup a working ghc (tested with v9.10) with WASM backend and wasm32-wasi-cabal.
2. `cd` into `wasm-submodules/network` and run `autoreconf -i`.
3. In the project root, run `wasm32-wasi-cabal build`.

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
If this does *not* occur to you or you can fix it, please let me know.

If everything works properly, it should build a `als.wasm`.
