# Agda Language Server

## Installation

The simplest way of acquiring Agda Language Server is through [agda-mode on VS Code](https://github.com/banacorn/agda-mode-vscode#agda-language-server).
Follow the instructions and the language server should be installed within seconds.

### Prebuilt binaries

You can also download prebuilt binaries [from the release page](https://github.com/banacorn/agda-language-server/releases) if you are using other LSP-compatible text editors.

Supported platforms: **Windows**, **Mac**, and **Ubuntu**.

### Build from source

You will need [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) to build the project:

```
stack install
```

## Versioning

The version is _x.y.z.w.a.b.c.d_ where _x.y.z.w_ is the version of the Agda Language Server and _a.b.c.d_ the version of Agda it embeds.
It follows the Haskell PVP (package versioning policy).

## Why make it standalone?

* for less impact on the Agda codebase
* to help [decouple the Agda codebase](https://github.com/agda/agda/projects/5)
* we can always merge it back to Agda later anyway

## Hacking 

This language server is co-developed alongside [agda-mode on VS Code](https://github.com/banacorn/agda-mode-vscode). 
Enable `agdaMode.connection.agdaLanguageServer` in agda-mode's settings, and then hit *restart* <kbd>C-x</kbd> <kbd>C-r</kbd> to connect to the language server. 
The editor extension will search for the language server in the following order:
1. `localhost:4096` via TCP
2. `als` executable on your machine
3. Prebuilt binaries on GitHub

To host the language server locally at `localhost:4096`, run `:main -p` in the REPL (`stack repl`). 
This allows us to reload the language server in the REPL without having to recompile and reinstall the whole project on your system every time there is a change.
