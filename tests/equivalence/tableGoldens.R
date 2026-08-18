#!/usr/bin/env Rscript
# Golden-table matrix generator (R side) for the Phase-2 Python equivalence work.
#
# Each case builds a jaspTable through the RCPP_MODULE ingest path (the same
# code real JASP modules use) and dumps the results JSON + toHtml. The Python
# twin (tableGoldens.py) builds the *same logical table* through the pybind
# adapter; the two outputs must be byte-identical (see runTableGoldens.sh).
#
# Usage:
#   JASP_EQUIV_RLIB=/path/to/lib Rscript tableGoldens.R [outdir]

if (Sys.getenv("JASP_EQUIV_RLIB") != "")
	.libPaths(c(Sys.getenv("JASP_EQUIV_RLIB"), .libPaths()))

suppressPackageStartupMessages(library(jaspBase))

ns <- asNamespace("jaspBase")
get0 <- function(n) get(n, envir=ns)

outdir <- {
	args <- commandArgs(trailingOnly=TRUE)
	if (length(args) >= 1) args[[1]] else file.path(getwd(), "tests", "equivalence", "tableGoldens")
}
dir.create(outdir, showWarnings=FALSE, recursive=TRUE)

get0("setDeveloperMode")(FALSE)

# Each case fills `t` (a fresh jaspTable). The harness wraps it in a jaspResults
# so we exercise the same dataEntry/meta/JSON machinery as production.
runCase <- function(name, buildTable)
{
	get0("destroyAllAllocatedObjects")()
	results <- get0("create_cpp_jaspResults")("goldens", NULL)
	get0("setResponseData")(7L, 0L)
	results$setOptions('{"case":"placeholder"}')

	t <- get0("create_cpp_jaspTable")(name)
	buildTable(t)
	results[["table"]] <- t

	writeLines(results$getResults(), file.path(outdir, paste0(name, "_results.json")))
	writeLines(t$toHtml(),           file.path(outdir, paste0(name, "_toHtml.txt")))
	cat("  R case ok:", name, "\n")
}

# --- cases -------------------------------------------------------------------

runCase("01_numeric_string", function(t) {
	t$setData(list(x=c(1.5, NA, Inf), s=c("a<b", "c&d", NA)))
	t$status <- "complete"
})

runCase("02_int_lgl", function(t) {
	t$setData(list(i=c(1L, 2L, NA), b=c(TRUE, NA, FALSE)))
	t$status <- "complete"
})

runCase("03_nan", function(t) {
	t$setData(list(v=c(1.0, NaN, NA)))
	t$status <- "complete"
})

runCase("04_row_names", function(t) {
	t$setData(data.frame(x=c(1,2,3), row.names=c("r1","r2","r3")))
	t$status <- "complete"
})

runCase("05_col_metadata", function(t) {
	t$setData(list(a=c(1,2), b=c("x","y")))
	t$setColTitles(list("Alpha", "Beta"))
	t$setColTypes(list("number", "string"))
	t$setColFormats(list("sf:4", ""))
	t$status <- "complete"
})

runCase("06_footnotes", function(t) {
	t$setData(list(x=c(1,2), y=c(3,4)))
	t$addFootnoteHelper("table note", NULL, NULL, NULL)
	t$addFootnoteHelper("col note", "*", "x", NULL)
	t$addFootnoteHelper("cell note", "#", "y", "2")
	t$status <- "complete"
})

runCase("07_add_column_info", function(t) {
	t$addColumnInfoHelper("m", "Mean", "number", "sf:4", TRUE, "Stats")
	t$addColumnInfoHelper("p", "p", "pvalue", "dp:3", NULL, "Stats")
	t$setData(list(m=c(1.234), p=c(0.041)))
	t$status <- "complete"
})

runCase("08_mixed_column", function(t) {
	mx <- createMixedColumn(list(1.5, "txt", 2L), c("number", "string", "integer"), list("sf:4", NULL, ""))
	t[["mx"]] <- mx
	t$status <- "complete"
})

runCase("09_add_rows", function(t) {
	t$setData(data.frame(x=numeric(0), y=numeric(0)))
	t$addRows(list(c(1, 10), c(2, 20), c(3, 30)))
	t$status <- "complete"
})

runCase("10_add_row_named", function(t) {
	t$setData(data.frame(x=numeric(0), y=numeric(0)))
	t$addRow(c(5, 6), "onlyrow")
	t$status <- "complete"
})

runCase("11_add_columns", function(t) {
	t$setData(list(a=c(1,2)))
	t$addColumns(list(b=c(3,4), c=c("u","v")))
	t$status <- "complete"
})

runCase("12_transpose", function(t) {
	t$setData(list(x=c(1,2), y=c(3,4)))
	t$setRowNames(list("r1","r2"))
	t$transpose <- TRUE
	t$status <- "complete"
})

runCase("13_expected_size", function(t) {
	t$setExpectedSize(3, 2)
	t$setData(list(x=c(1,2)))
	t$status <- "running"
})

runCase("14_matrix_input", function(t) {
	# matrix() fills column-major: same logical layout as the Python twin
	# (np.array rows [[1,4],[2,5],[3,6]] -> columns c1=[1,2,3], c2=[4,5,6]).
	m <- matrix(c(1, 2, 3, 4, 5, 6), nrow=3, ncol=2)
	colnames(m) <- c("c1", "c2")
	t$setData(m)
	t$status <- "complete"
})

runCase("15_vector_row", function(t) {
	t$setData(c(1.5, 2.5, 3.5))
	t$status <- "complete"
})

runCase("16_list_of_columns", function(t) {
	t$setData(list(a=c(1,2,3), b=c("x","y","z")))
	t$status <- "complete"
})

runCase("17_escape_and_citation", function(t) {
	t$setData(list(s=c("Tom & Jerry", "<script>", "1 < 2")))
	t$addCitation("A citation with <tags> & symbols")
	t$status <- "complete"
})

runCase("18_empty_table", function(t) {
	t$status <- "complete"
})

runCase("19_dataframe_rownames", function(t) {
	# data.frame carries implicit row names "1".."n"; the Python twin passes
	# them explicitly (documented §3.2 difference: no implicit index names).
	t$setData(data.frame(x=c(1,2,3), s=c("a","b","c"), stringsAsFactors=FALSE))
	t$status <- "complete"
})

runCase("20_factor_categorical", function(t) {
	t$setData(data.frame(f=factor(c("a","b",NA,"a")), v=c(1,2,3,4)))
	t$status <- "complete"
})

cat("R goldens written to", outdir, "\n")
