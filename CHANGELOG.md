# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## v0.2.7.0.1.2 - 2024-12-4

### Fixed
- Release of artefacts on macOS

## v0.2.7.0.1.1 - 2024-12-4

### Added
- #29: Integration testing for the language server
- Prebuilt binaries for Intel x64 macOS 
- Support for Agda-2.7.0.1

### Changed
- Unfied workflow for building and testing the language server across all platforms

## v0.2.7.0.1.0 - 2024-12-2

### Added
- #22: agda 2.6.4 by [@andreasabel](https://github.com/andreasabel)
- #23: lsp 2 by [@andreasabel](https://github.com/andreasabel)

### Fixed
- Version information of the language server

## v0.2.6.4.0.3 - 2023-12-14

### Fixed
- #15: Add missing handlers for `lsp` methods.
- #24: Fix the encoding of binaries built on GitHub Actions.
- Patch path to the "data" directory when the executable is built on GitHub Actions.

## v0.2.6.4.0.0 - 2023-12-12

### Changed
- Embed Agda-2.6.4.
- Builds with `lsp` < 1.7 on GHC 9.2 (LTS 20.26),
  and with Cabal also on 9.4 and 9.6.

### Added
- Build flag `Agda-2-6-3` to embed Agda-2.6.3 rather than 2.6.4.


## v0.2.6.3.0 - 2023-11-23

### Changed
- Embed Agda-2.6.3.
- Builds with `lsp` < 1.7 on GHC 8.10 (LTS 18.28), 9.0 (LTS 19.33), and 9.2 (LTS 20.26),
  and with Cabal also on 9.4 and 9.6.

### Added
- Build flag `Agda-2-6-2-2` to embed Agda-2.6.2.2 rather than 2.6.3.


## v0.2.6.2.2.1 - 2023-11-21

### Added

- Building with `lsp-1.6`.
  Builds with `lsp` < 1.7 on GHC 8.10 (LTS 18.28), 9.0 (LTS 19.33), and 9.2 (LTS 20.26).


## v0.2.6.2.2 - 2023-11-21

### Changed

- Embed Agda-2.6.2.2.
- Versioning scheme: _x.a.b.c.d.y_ where _a.b.c.d_ is the 4-digit Agda version (2.6.2.2), _x_ is 0 but may be bumped for revolutionary changes to the agda-language-server, and _y_ is for patch releases.
- Builds with `lsp` < 1.5 on GHC 8.10 (LTS 18.28) and 9.0 (LTS 19.33).


## v0.2.1 - 2021-10-25

No changes.


## v0.2.0 - 2021-10-22

### Fixed
- #2: Allow user to supply command-line options via agda-mode


## v0.1.4 - 2021-10-04

### Fixed
- Resume sending HighlightingInfos to agda-mode


## v0.1.3 - 2021-10-04

### Fixed
- Include DLLs in the bundle


## v0.1.2 - 2021-10-03

### Fixed
- #5: Connection Error
