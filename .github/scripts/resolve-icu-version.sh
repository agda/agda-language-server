#!/usr/bin/env bash
# Resolves the ICU ABI version used for the compiled-dependency cache key
# (redesigned policy) and exports ICU_VERSION. Falls back to "unknown"
# rather than failing, since ICU version is only load-bearing for the
# redesigned cache key, not for the legacy policy or the build itself.
set -euo pipefail

version="unknown"
if command -v pkg-config >/dev/null 2>&1 && pkg-config --exists icu-uc 2>/dev/null; then
  version="$(pkg-config --modversion icu-uc)"
elif [[ "$RUNNER_OS" == "Windows" ]] && command -v pkgconf >/dev/null 2>&1 && pkgconf --exists icu-uc 2>/dev/null; then
  version="$(pkgconf --modversion icu-uc)"
fi

echo "ICU_VERSION=${version}" >> "$GITHUB_ENV"
