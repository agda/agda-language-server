#!/usr/bin/env bash
# Checks that a packaged 'als' binary can start.
#
# Usage: check-artifact-runs.sh <path-to-als> [--expect-failure] [--hide-icu]
#
# --hide-icu hides the system ICU library (via an overlay mount in a
# throwaway mount namespace, root required) to reproduce #6.
set -u

als="${1:?usage: check-artifact-runs.sh <path-to-als> [--expect-failure] [--hide-icu]}"
shift
expect_failure=false
hide_icu=false
for arg in "$@"; do
  case "$arg" in
    --expect-failure) expect_failure=true ;;
    --hide-icu) hide_icu=true ;;
    *) echo "unknown argument: $arg" >&2; exit 2 ;;
  esac
done

if $hide_icu; then
  icu_symlink=$(ldd "$als" | grep libicuuc | awk '{print $3}')
  if [[ -z "$icu_symlink" ]]; then
    echo "could not determine which libicuuc.so '$als' is linked against" >&2
    exit 2
  fi
  icu_real_dir=$(dirname "$(readlink -f "$icu_symlink")")

  self="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
  extra_arg=""
  $expect_failure && extra_arg="--expect-failure"

  exec sudo unshare --mount --propagation private -- bash -c "
    set -e
    mkdir -p /tmp/icu-ovl/upper /tmp/icu-ovl/work /tmp/icu-ovl/merged
    mount -t overlay overlay -o lowerdir=$icu_real_dir,upperdir=/tmp/icu-ovl/upper,workdir=/tmp/icu-ovl/work /tmp/icu-ovl/merged
    rm -f /tmp/icu-ovl/merged/libicu*
    mount --bind /tmp/icu-ovl/merged $icu_real_dir
    bash $self $als $extra_arg
  "
fi

log="$(mktemp)"
TIMEOUT=timeout; command -v timeout >/dev/null 2>&1 || TIMEOUT=gtimeout
if command -v "$TIMEOUT" >/dev/null 2>&1; then
  "$TIMEOUT" 10 "$als" --version > "$log" 2>&1
else
  "$als" --version > "$log" 2>&1
fi
status=$?

echo "--- $als --version (exit $status) ---"
cat "$log"
echo "-----------------------------------------"

if ! $expect_failure; then
  if [[ $status -eq 0 ]]; then
    echo "PASS: '$als' started successfully"
    exit 0
  fi
  echo "FAIL: expected '$als' to start successfully, but it exited $status"
  exit 1
fi

if [[ $status -eq 0 ]]; then
  echo "FAIL: expected '$als' to fail to start (no bundled ICU), but it ran successfully"
  exit 1
fi
if ! grep -qi "cannot open shared object file" "$log"; then
  echo "FAIL: '$als' failed, but not with the ICU dynamic-linking error reported in #6"
  exit 1
fi
echo "PASS: '$als' failed to start exactly as #6 describes"
