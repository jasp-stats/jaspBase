#!/usr/bin/env bash
# Phase-2 golden-table equivalence gate.
#
# Builds the Python extension (if needed), runs the R and Python golden-table
# generators, and byte-compares every produced file. Exits non-zero on any
# difference.
#
#   tests/equivalence/runTableGoldens.sh            # build + run + compare
#   tests/equivalence/runTableGoldens.sh --skip-build
set -euo pipefail

EQ_DIR="$(cd "$(dirname "$0")" && pwd)"
PKG_ROOT="$(cd "$EQ_DIR/../.." && pwd)"
RLIB="${JASP_EQUIV_RLIB:-/tmp/opencode/jaspBaseLib}"
PY_DIR="$PKG_ROOT/python"
PY_MODULE_DIR="$PY_DIR/build"
SPIKE_PY="$PKG_ROOT/tmp/spike/.venv/bin/python"
SCRATCH="$(mktemp -d /tmp/jaspTableGoldens.XXXXXX)"
trap 'rm -rf "$SCRATCH"' EXIT

if [ "${1:-}" != "--skip-build" ]; then
	echo "==> building python extension into $PY_MODULE_DIR"
	cmake -S "$PY_DIR" -B "$PY_MODULE_DIR" -DCMAKE_BUILD_TYPE=Release \
		-Dpybind11_DIR="$("$SPIKE_PY" -m pybind11 --cmakedir)" >/dev/null
	cmake --build "$PY_MODULE_DIR" -j >/dev/null
fi

echo "==> generating R goldens"
JASP_EQUIV_RLIB="$RLIB" Rscript "$EQ_DIR/tableGoldens.R" "$SCRATCH/r" >/dev/null

echo "==> generating Python goldens"
JASP_PY_MODULE_DIR="$PY_MODULE_DIR" "$SPIKE_PY" "$EQ_DIR/tableGoldens.py" "$SCRATCH/py" >/dev/null

echo "==> comparing"
fail=0
for f in "$SCRATCH/r"/*; do
	base="$(basename "$f")"
	if [ ! -f "$SCRATCH/py/$base" ]; then
		echo "  $base  MISSING on python side"
		fail=1
		continue
	fi
	if cmp -s "$f" "$SCRATCH/py/$base"; then
		echo "  $base  IDENTICAL"
	else
		echo "  $base  DIFFER:"
		diff "$f" "$SCRATCH/py/$base" | head -20
		fail=1
	fi
done

# also flag python-only files
for f in "$SCRATCH/py"/*; do
	base="$(basename "$f")"
	[ -f "$SCRATCH/r/$base" ] || { echo "  $base  MISSING on R side"; fail=1; }
done

[ "$fail" -eq 0 ] && echo "TABLE GOLDENS BYTE-IDENTICAL" || { echo "TABLE GOLDEN MISMATCH"; exit 1; }
