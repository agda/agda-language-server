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

The version is _x.a.b.c.d.y_ where _a.b.c.d_ is the 4-digit Agda version (2.6.4.0), _x_ is 0 but may be bumped for revolutionary changes to the agda-language-server, and _y_ is for patch releases.

## Why make it standalone?

* for less impact on the Agda codebase
* to help [decouple the Agda codebase](https://github.com/agda/agda/projects/5)
* we can always merge it back to Agda later anyway
