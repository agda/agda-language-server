#!/usr/bin/env bash
# validate-native-utils.sh
#
# Resolves alex/happy to canonical paths, asserts both are inside the
# restored native-utilities payload (NATIVE_UTILS_DIR, default
# $HOME/.native-utils, kept outside the .ghc-wasm toolchain payload so the
# two caches never overlap), and asserts both report the pinned
# ALEX_VERSION/HAPPY_VERSION. Fails the job on any mismatch rather than
# silently accepting a runner-provided binary that happens to be on PATH.
#
# Also actually runs each tool against a trivial input, not just
# --version: alex/happy need their datadir (AlexTemplate.hs /
# HappyTemplate.hs etc., resolved via alex_datadir/happy_datadir env vars
# or a build-time-baked Cabal store path) to generate anything, and a
# version check alone does not exercise that path.
set -euo pipefail

payload_dir="${NATIVE_UTILS_DIR:-$HOME/.native-utils}"

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

workdir="$(mktemp -d)"
trap 'rm -rf "$workdir"' EXIT

cat > "$workdir/Test.x" <<'EOF'
{
module Main (main) where
}
%wrapper "basic"
tokens :-
  $white+ ;
  a       { \_ -> "A" }
{
main :: IO ()
main = print (alexScanTokens "a")
}
EOF
alex "$workdir/Test.x" -o "$workdir/Test.hs" || {
  echo "::error::alex failed to generate a lexer from a trivial input (likely missing its datadir)" >&2
  exit 1
}
echo "alex: generated a lexer from a trivial input OK"

cat > "$workdir/Test.y" <<'EOF'
{
module Main (main) where
}
%name parseA
%tokentype { Char }
%error { \_ -> error "parse error" }
%token
  a { 'a' }
%%
A : a { $1 }
{
main :: IO ()
main = print (parseA "a")
}
EOF
happy "$workdir/Test.y" -o "$workdir/TestParser.hs" || {
  echo "::error::happy failed to generate a parser from a trivial input (likely missing its datadir)" >&2
  exit 1
}
echo "happy: generated a parser from a trivial input OK"
