# Agda Language Server

## Supported versions of Agda

These are the currently supported versions of Agda:
* Agda-2.7.0.1
* Agda-2.6.4.3
* Agda-2.6.3

All releases will come with binaries built with these versions of Agda.

We plan to make the codebase compatible with **at most 3 versions of Agda** at a single time. Because otherwise we'd be drowned by CPP macros for conditional compilation.

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

Single-digit versioning for the language server itself - people mostly just care about the Agda version it supports anyway.

## Why make it standalone?

* for less impact on the Agda codebase
* to help [decouple the Agda codebase](https://github.com/agda/agda/projects/5)
* we can always merge it back to Agda later anyway

## Hacking

This language server is co-developed alongside [agda-mode on VS Code](https://github.com/banacorn/agda-mode-vscode).

### TCP server for development

To make development easier, you can host the language server locally at `localhost:4096` by running `:main -p` in the REPL (`stack repl`).

Add `lsp://localhost:4096` to `agdaMode.connection.paths` in VS Code's settings, then hit <kbd>C-x</kbd> <kbd>C-s</kbd> to select it as the endpoint.

This allows you to reload the language server in the REPL without recompiling and reinstalling the whole project every time you make changes.

