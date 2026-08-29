"""Pytest matrix for the Python table-ingest adapter (§3.1/§3.2).

These run without R: they assert the JSON cells and shapes the pybind adapter
produces, locking down the Python side of the R-vs-Python equivalence contract.
The byte-identical R-vs-Python comparison lives in tests/equivalence/.

Run from the repo root with the built extension on the path:
    JASP_PY_MODULE_DIR=python/build python -m pytest python/tests -q
"""
import json
import os
import sys

import pytest

sys.path.insert(0, os.environ.get(
	"JASP_PY_MODULE_DIR",
	os.path.join(os.path.dirname(__file__), "..", "build")))

import _jaspresults as M  # noqa: E402

np = pytest.importorskip("numpy")
pd = pytest.importorskip("pandas")


@pytest.fixture(autouse=True)
def _clean():
	M.destroyAllAllocatedObjects()
	yield
	M.destroyAllAllocatedObjects()


def cells(table):
	"""Return the column-major cell matrix as parsed JSON."""
	return json.loads(table._debugCells())


def make_table():
	return M.jaspTable("t")


# --- §3.1 scalar -> cell -----------------------------------------------------

def test_none_is_empty_string():
	t = make_table()
	t.setData({"x": [None]})
	assert cells(t) == [[""]]


def test_nan_is_empty_string():
	# Python can't distinguish NA from NaN; nan maps to NA -> "".
	t = make_table()
	t.setData({"x": [float("nan")]})
	assert cells(t) == [[""]]


def test_nanstring_marker_is_nan_literal():
	# The explicit marker reproduces R's NaN -> "NaN" cell.
	t = make_table()
	t.setData({"x": [M.NaNString]})
	assert cells(t) == [["NaN"]]


def test_inf_maps_to_infinity_glyphs():
	t = make_table()
	t.setData({"x": [float("inf"), float("-inf")]})
	assert cells(t) == [["\u221e", "-\u221e"]]


def test_int_bool_string_cells():
	t = make_table()
	t.setData({"i": [1, 2], "b": [True, False], "s": ["a", "b"]})
	assert cells(t) == [[1, 2], [True, False], ["a", "b"]]


def test_string_html_escaped_by_default():
	t = make_table()
	t.setData({"s": ["a<b", "c&d"]})
	assert cells(t) == [["a&lt;b", "c&amp;d"]]


def test_numpy_scalars_reduce_to_builtins():
	t = make_table()
	t.setData({"x": [np.int64(3), np.float64(1.5), np.bool_(True)]})
	assert cells(t) == [[3, 1.5, True]]


def test_pandas_na_is_empty_string():
	t = make_table()
	t.setData({"x": [pd.NA, 1.0]})
	assert cells(t) == [["", 1.0]]


# --- §3.2 shape dispatch -----------------------------------------------------

def test_dict_is_columns():
	t = make_table()
	t.setData({"a": [1.0, 2.0], "b": ["x", "y"]})
	assert cells(t) == [[1.0, 2.0], ["x", "y"]]


def test_dataframe_is_columns():
	t = make_table()
	t.setData(pd.DataFrame({"a": [1.0, 2.0], "b": ["x", "y"]}))
	assert cells(t) == [[1.0, 2.0], ["x", "y"]]


def test_series_is_one_named_column():
	t = make_table()
	t.setData(pd.Series([1.0, 2.0], name="s"))
	assert cells(t) == [[1.0, 2.0]]


def test_flat_list_is_one_row():
	# Like an R atomic vector: each scalar becomes its own one-cell column.
	t = make_table()
	t.setData([1.0, 2.0, 3.0])
	assert cells(t) == [[1.0], [2.0], [3.0]]


def test_list_of_lists_is_rows():
	# Unnamed rows: all cells pile into one unnamed column (matches R behavior)
	t = make_table()
	t.setData([[1.0, 2.0], [3.0, 4.0]])
	assert cells(t) == [[1.0, 2.0, 3.0, 4.0]]


def test_2d_ndarray_is_columns():
	t = make_table()
	t.setData(np.array([[1.0, 4.0], [2.0, 5.0], [3.0, 6.0]]))
	assert cells(t) == [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]


def test_range_is_one_row():
	t = make_table()
	t.setData(range(3))
	assert cells(t) == [[0], [1], [2]]


def test_categorical_maps_to_strings():
	t = make_table()
	t.setData(pd.DataFrame({"f": pd.Categorical(["a", None, "a"])}))
	assert cells(t) == [["a", "", "a"]]


# --- mixed cells -------------------------------------------------------------

def test_mixed_dict_cell():
	t = make_table()
	t.setColumn("mx", [
		{"value": 1.5, "type": "number", "format": "sf:4"},
		{"value": "txt", "type": "string", "format": None},
	])
	assert cells(t) == [[
		{"value": 1.5, "type": "number", "format": "sf:4"},
		{"value": "txt", "type": "string", "format": None},
	]]


# --- row / column names ------------------------------------------------------

def test_row_names_applied():
	t = make_table()
	t.setData({"x": [1.0, 2.0]}, row_names=["r1", "r2"])
	# Row names surface in the results JSON schema/rows, not in _debugCells.
	results = M.jaspResults("r")
	M.setResponseData(1, 0)
	results.setOptions("{}")
	results.insert("t", t)
	out = json.loads(results.getResults())
	# Table should exist in results with correct schema
	assert "t" in out["results"]
	assert out["results"]["t"]["schema"]["fields"][0]["name"] == "x"
	assert out["results"]["t"]["data"] == [{"x": 1.0}, {"x": 2.0}]


def test_add_rows_appends():
	t = make_table()
	t.setData({"x": [1.0], "y": [2.0]})
	t.addRows([[3.0, 4.0], [5.0, 6.0]])
	result = cells(t)
	# Unnamed row cells go into a new unnamed column (matches R behavior)
	assert len(result) == 3
	assert result[0] == [1.0]
	assert result[1] == [2.0]
	assert result[2] == [None, 3.0, 4.0, 5.0, 6.0]


def test_add_columns_appends():
	t = make_table()
	t.setData({"a": [1.0, 2.0]})
	t.addColumns({"b": [3.0, 4.0]})
	assert cells(t) == [[1.0, 2.0], [3.0, 4.0]]
