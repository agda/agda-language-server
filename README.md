# agda-language-server

Language Server Protocol for Agda

To be developed alongside [agda-mode-vscode](https://github.com/banacorn/agda-mode-vscode)

## Roadmap

- [ ] Make it a simple relay between *Agda* and *agda-mode-vscode*
- [ ] Bypass the old Emacs protocol and access *Agda* directly

## Why make it standalone?

* for less impact on the Agda codebase
* to help [decouple the Agda codebase](https://github.com/agda/agda/projects/5)
* we can always merge it back to Agda later anyway
