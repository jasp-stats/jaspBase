#!/usr/bin/env python3
"""Golden-table matrix generator (Python side) for the Phase-2 equivalence work.

Twin of tableGoldens.R: each case builds the *same logical table* through the
pybind adapter's Python-native ingest (dict / list / ndarray) that the R side
builds through data.frame / list / matrix. Outputs must be byte-identical
(see runTableGoldens.sh).

Requires the built `_jaspresults` extension; point JASP_PY_MODULE_DIR at its
directory (defaults to python/build).
"""
import os
import sys

mod_dir = os.environ.get(
	"JASP_PY_MODULE_DIR",
	os.path.join(os.path.dirname(__file__), "..", "..", "python", "build"))
sys.path.insert(0, os.path.abspath(mod_dir))

import _jaspresults as M  # noqa: E402

outdir = sys.argv[1] if len(sys.argv) >= 1 + 1 else os.path.join(
	os.path.dirname(__file__), "tableGoldens")
os.makedirs(outdir, exist_ok=True)

M.setDeveloperMode(False)


def run_case(name, build_table):
	M.destroyAllAllocatedObjects()
	results = M.jaspResults("goldens")
	M.setResponseData(7, 0)
	results.setOptions('{"case":"placeholder"}')

	t = M.jaspTable(name)
	build_table(t)
	results.insert("table", t)

	with open(os.path.join(outdir, name + "_results.json"), "w") as f:
		f.write(results.getResults() + "\n")
	with open(os.path.join(outdir, name + "_toHtml.txt"), "w") as f:
		f.write(t.toHtml() + "\n")


def case_01(t):
	t.setData({"x": [1.5, None, float("inf")], "s": ["a<b", "c&d", None]})
	t.status = "complete"


def case_02(t):
	t.setData({"i": [1, 2, None], "b": [True, None, False]})
	t.status = "complete"


def case_03(t):
	t.setData({"v": [1.0, M.NaNString, None]})
	t.status = "complete"


def case_04(t):
	# R c(1,2,3) is double; use floats so the cell type matches ("number").
	t.setData({"x": [1.0, 2.0, 3.0]}, row_names=["r1", "r2", "r3"])
	t.status = "complete"


def case_05(t):
	t.setData({"a": [1.0, 2.0], "b": ["x", "y"]})
	t.setColTitles(["Alpha", "Beta"])
	t.setColTypes(["number", "string"])
	t.setColFormats(["sf:4", ""])
	t.status = "complete"


def case_06(t):
	t.setData({"x": [1.0, 2.0], "y": [3.0, 4.0]})
	t.addFootnote("table note")
	t.addFootnote("col note", symbol="*", col_names=["x"])
	t.addFootnote("cell note", symbol="#", col_names=["y"], row_names=["2"])
	t.status = "complete"


def case_07(t):
	t.addColumnInfo("m", "Mean", "number", "sf:4", True, "Stats")
	t.addColumnInfo("p", "p", "pvalue", "dp:3", None, "Stats")
	t.setData({"m": [1.234], "p": [0.041]})
	t.status = "complete"


def case_08(t):
	t.setColumn("mx", [
		{"value": 1.5, "type": "number", "format": "sf:4"},
		{"value": "txt", "type": "string", "format": None},
		{"value": 2, "type": "integer", "format": ""},
	])
	t.status = "complete"


def case_09(t):
	t.setData({"x": [], "y": []})
	t.addRows([[1.0, 10.0], [2.0, 20.0], [3.0, 30.0]])
	t.status = "complete"


def case_10(t):
	t.setData({"x": [], "y": []})
	t.addRow([5.0, 6.0], row_name="onlyrow")
	t.status = "complete"


def case_11(t):
	t.setData({"a": [1.0, 2.0]})
	t.addColumns({"b": [3.0, 4.0], "c": ["u", "v"]})
	t.status = "complete"


def case_12(t):
	t.setData({"x": [1.0, 2.0], "y": [3.0, 4.0]})
	t.setRowNames(["r1", "r2"])
	t.transpose = True
	t.status = "complete"


def case_13(t):
	t.setExpectedSize(3, 2)
	t.setData({"x": [1.0, 2.0]})
	t.status = "running"


def case_14(t):
	# 2-D ndarray -> columns (like an R matrix)
	np = __import__("numpy")
	arr = np.array([[1, 4], [2, 5], [3, 6]], dtype=float)
	t.setData(arr, col_names=["c1", "c2"])
	t.status = "complete"


def case_15(t):
	t.setData([1.5, 2.5, 3.5])
	t.status = "complete"


def case_16(t):
	t.setData({"a": [1.0, 2.0, 3.0], "b": ["x", "y", "z"]})
	t.status = "complete"


def case_17(t):
	t.setData({"s": ["Tom & Jerry", "<script>", "1 < 2"]})
	t.addCitation("A citation with <tags> & symbols")
	t.status = "complete"


def case_18(t):
	t.status = "complete"


def case_19(t):
	# R data.frame carries implicit row names "1".."n"; Python passes them
	# explicitly (documented §3.2 difference: no implicit index names).
	t.setData({"x": [1.0, 2.0, 3.0], "s": ["a", "b", "c"]}, row_names=["1", "2", "3"])
	t.status = "complete"


def case_20(t):
	# R factor -> character column; pandas Categorical -> category values.
	# R side also has a numeric column v=c(1,2,3,4) (doubles).
	pd = __import__("pandas")
	df = pd.DataFrame({
		"f": pd.Categorical(["a", "b", None, "a"]),
		"v": [1.0, 2.0, 3.0, 4.0],
	})
	# R data.frames carry implicit row names "1".."n"; pass them explicitly.
	t.setData(df, row_names=["1", "2", "3", "4"])
	t.status = "complete"


CASES = {
	"01_numeric_string": case_01,
	"02_int_lgl": case_02,
	"03_nan": case_03,
	"04_row_names": case_04,
	"05_col_metadata": case_05,
	"06_footnotes": case_06,
	"07_add_column_info": case_07,
	"08_mixed_column": case_08,
	"09_add_rows": case_09,
	"10_add_row_named": case_10,
	"11_add_columns": case_11,
	"12_transpose": case_12,
	"13_expected_size": case_13,
	"14_matrix_input": case_14,
	"15_vector_row": case_15,
	"16_list_of_columns": case_16,
	"17_escape_and_citation": case_17,
	"18_empty_table": case_18,
	"19_dataframe_rownames": case_19,
	"20_factor_categorical": case_20,
}

for name, fn in CASES.items():
	run_case(name, fn)

print("Python goldens written to", outdir)
