#!/usr/bin/env bash
# validate-native-utils.sh
#
# Resolves alex/happy to canonical paths, asserts both are inside the
# restored native-utilities payload (NATIVE_UTILS_DIR, default
# $HOME/.ghc-wasm/native-utils), and asserts both report the pinned
# ALEX_VERSION/HAPPY_VERSION. Fails the job on any mismatch rather than
# silently accepting a runner-provided binary that happens to be on PATH.
set -euo pipefail

payload_dir="${NATIVE_UTILS_DIR:-$HOME/.ghc-wasm/native-utils}"

check_tool() {
  local name="$1" expected_version="$2" version_flag="$3"
  local path
  path="$(command -v "$name")" || {
    echo "::error::${name} not found on PATH" >&2
    exit 1
  }
  path="$(cd "$(dirname "$path")" && pwd)/$(basename "$path")"

  case "$path" in
    "${payload_dir}"/*) ;;
    *)
      echo "::error::${name} resolved to '${path}', which is outside the restored native-utilities payload '${payload_dir}'" >&2
      exit 1
      ;;
  esac

  local actual_version
  actual_version="$("$name" "$version_flag" 2>&1 | grep -oE '[0-9]+\.[0-9]+(\.[0-9]+)*' | head -1)"
  if [[ "$actual_version" != "$expected_version" ]]; then
    echo "::error::${name} at ${path} reports version '${actual_version}', expected '${expected_version}'" >&2
    exit 1
  fi
  echo "${name}: ${path} (${actual_version}) OK"
}

check_tool alex "$ALEX_VERSION" --version
check_tool happy "$HAPPY_VERSION" --version
