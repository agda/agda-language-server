#!/usr/bin/env bash
# validate-wasm-toolchain.sh
#
# Part of the .ghc-wasm toolchain producer: a ghc-wasm-meta setup only
# counts as a successful producer once the resulting toolchain executables
# actually work, not merely because setup.sh exited zero.
#
# Runs on both cache hits and misses, so a payload that was fine when
# saved but has since drifted (or was restored from a stale/incompatible
# key) is still caught. Checks against wasm-toolchain-manifest.env, an
# independent, checked-in manifest read from the pinned ghc-wasm-meta
# commit's own release metadata -- never against whatever this run
# happens to have just installed, which would make the check tautological.
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=wasm-toolchain-manifest.env
source "${script_dir}/wasm-toolchain-manifest.env"

if [[ "${GHC_WASM_META_COMMIT_HASH:-}" != "${EXPECTED_GHC_WASM_COMMIT}" ]] ||
   [[ "${GHC_WASM_META_FLAVOUR:-}" != "${EXPECTED_GHC_WASM_FLAVOUR}" ]]; then
  echo "::error::wasm-toolchain-manifest.env is stale: pinned commit/flavour" \
    "('${GHC_WASM_META_COMMIT_HASH:-}', '${GHC_WASM_META_FLAVOUR:-}') no longer" \
    "matches the manifest ('${EXPECTED_GHC_WASM_COMMIT}', '${EXPECTED_GHC_WASM_FLAVOUR}')." \
    "Update the manifest from the new commit's own ghcup-wasm-*.yaml." >&2
  exit 1
fi

payload_dir="${WASM_TOOLCHAIN_DIR:-$HOME/.ghc-wasm}"
payload_dir="$(cd "${payload_dir}" && pwd)"

check_tool() {
  local name="$1"
  local path
  path="$(command -v "$name")" || {
    echo "::error::${name} not found on PATH" >&2
    exit 1
  }
  local real
  real="$(readlink -f "$path")"

  case "$real" in
    "${payload_dir}"/*) ;;
    *)
      echo "::error::${name} resolved to '${real}', which is outside the restored toolchain payload '${payload_dir}'" >&2
      exit 1
      ;;
  esac
  echo "${name}: ${path} -> ${real}"
}

check_tool wasm32-wasi-ghc
check_tool wasm32-wasi-cabal

actual_ghc_version="$(wasm32-wasi-ghc --numeric-version)"
if [[ "${actual_ghc_version}" != "${EXPECTED_GHC_NUMERIC_VERSION}" ]]; then
  echo "::error::wasm32-wasi-ghc reports version '${actual_ghc_version}', expected '${EXPECTED_GHC_NUMERIC_VERSION}' per wasm-toolchain-manifest.env" >&2
  exit 1
fi

actual_target="$(wasm32-wasi-ghc --info | grep -oE '"Target platform",\s*"[^"]*"' | grep -oE '"[^"]*"$' | tr -d '"')"
if [[ "${actual_target}" != *"${EXPECTED_GHC_TARGET_TRIPLE}"* ]]; then
  echo "::error::wasm32-wasi-ghc target platform '${actual_target}' does not contain expected '${EXPECTED_GHC_TARGET_TRIPLE}'" >&2
  exit 1
fi

echo "wasm32-wasi-ghc: ${actual_ghc_version} (${actual_target}) OK, matches manifest"
wasm32-wasi-cabal --version
