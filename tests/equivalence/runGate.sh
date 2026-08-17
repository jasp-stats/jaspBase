#!/usr/bin/env bash
# Phase-1 equivalence gate.
#
# Rebuilds jaspBase from the CURRENT working tree into a temp library, regenerates
# the module fingerprint + golden JSONs, and diffs them against the committed
# fixtures in tests/equivalence/fixtures/. Exits non-zero on any difference.
#
#   tests/equivalence/runGate.sh            # full: rebuild + compare
#   JASP_EQUIV_RLIB=/existing/lib tests/equivalence/runGate.sh --skip-build
#
# The fingerprint must be byte-identical; the goldens must be byte-identical.
set -euo pipefail

EQ_DIR="$(cd "$(dirname "$0")" && pwd)"
PKG_ROOT="$(cd "$EQ_DIR/../.." && pwd)"
RLIB="${JASP_EQUIV_RLIB:-/tmp/opencode/jaspBaseLib}"
SCRATCH="$(mktemp -d /tmp/jaspEquiv.XXXXXX)"
trap 'rm -rf "$SCRATCH"' EXIT

if [ "${1:-}" != "--skip-build" ]; then
	echo "==> staging Common + installing jaspBase (working tree) into $RLIB"
	mkdir -p "$PKG_ROOT/inst/include"
	rm -rf "$PKG_ROOT/inst/include/Common"
	cp -R "$PKG_ROOT/../../Common" "$PKG_ROOT/inst/include/Common"
	mkdir -p "$RLIB"
	( cd "$PKG_ROOT" && R CMD INSTALL --library="$RLIB" \
		--configure-vars="INCLUDE_DIR=$PKG_ROOT/inst/include/Common" . >/dev/null )
fi

export JASP_EQUIV_RLIB="$RLIB"

echo "==> regenerating fingerprint + goldens into $SCRATCH"
Rscript "$EQ_DIR/moduleFingerprint.R" "$SCRATCH/moduleFingerprint.txt" >/dev/null
Rscript "$EQ_DIR/goldenBaseline.R"    "$SCRATCH" >/dev/null
Rscript "$EQ_DIR/toRObjectBaseline.R" "$SCRATCH/toRObject_fingerprint.txt" >/dev/null

echo "==> comparing against committed fixtures"
fail=0
for f in moduleFingerprint.txt golden_response.json golden_pruned.json golden_saved.json toRObject_fingerprint.txt; do
	if cmp -s "$EQ_DIR/fixtures/$f" "$SCRATCH/$f"; then
		echo "  $f  IDENTICAL"
	else
		echo "  $f  DIFFER:"
		diff "$EQ_DIR/fixtures/$f" "$SCRATCH/$f" | head -25
		fail=1
	fi
done

[ "$fail" -eq 0 ] && echo "GATE PASSED" || { echo "GATE FAILED"; exit 1; }
