#!/usr/bin/env bash
set -euo pipefail

project_root="${1:-$PWD}"

serena_bin=""
uvx_bin=""

if command -v serena >/dev/null 2>&1; then
  serena_bin="$(command -v serena)"
elif [[ -x "${HOME}/.local/bin/serena" ]]; then
  serena_bin="${HOME}/.local/bin/serena"
fi

if command -v uvx >/dev/null 2>&1; then
  uvx_bin="$(command -v uvx)"
elif [[ -x "${HOME}/.local/bin/uvx" ]]; then
  uvx_bin="${HOME}/.local/bin/uvx"
fi

# Keep uv cache in a writable location when running under sandboxed shells.
if [[ -z "${UV_CACHE_DIR:-}" ]]; then
  export UV_CACHE_DIR="${TMPDIR:-${HOME}/.cache}/uv"
fi
if [[ -z "${XDG_CACHE_HOME:-}" ]]; then
  export XDG_CACHE_HOME="${TMPDIR:-${HOME}/.cache}"
fi

if [[ -n "${serena_bin}" ]]; then
  exec "${serena_bin}" start-mcp-server --context ide --project "$project_root"
fi

# Offline-first fallback: use cached Serena wheel from uv cache when available.
if [[ -n "${uvx_bin}" ]]; then
  cached_wheel="$(find "${UV_CACHE_DIR}" "${HOME}/.cache/uv/sdists-v9" -type f -name 'serena_agent-*.whl' 2>/dev/null | head -n 1 || true)"
  if [[ -n "${cached_wheel}" ]]; then
    exec "${uvx_bin}" --offline --from "${cached_wheel}" serena start-mcp-server --context ide --project "$project_root"
  fi
fi

if [[ -n "${uvx_bin}" ]]; then
  exec "${uvx_bin}" --from git+https://github.com/oraios/serena serena start-mcp-server --context ide --project "$project_root"
fi

echo "Serena launcher not found. Install 'serena' or 'uvx' to run the MCP server." >&2
exit 127
