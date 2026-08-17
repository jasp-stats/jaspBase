#!/usr/bin/env Rscript
# Golden baseline generator for the core/adapters refactor (Phase 1 gate).
#
# Builds a rich jaspResults tree exercising every object type and both JSON
# serialisation paths, then writes:
#   fixtures/golden_response.json   <- results$getResults()  (dataEntry/meta tree)
#   fixtures/golden_saved.json      <- saveResults() file    (convertToJSON tree)
#
# After any refactor, rebuild jaspBase, re-run this script into a scratch dir and
# diff against the committed fixtures: they must be byte-identical.
#
# Usage:
#   JASP_EQUIV_RLIB=/path/to/lib Rscript goldenBaseline.R [outdir]
#   (outdir defaults to tests/equivalence/fixtures)

if (Sys.getenv("JASP_EQUIV_RLIB") != "")
	.libPaths(c(Sys.getenv("JASP_EQUIV_RLIB"), .libPaths()))

suppressPackageStartupMessages(library(jaspBase))

ns <- asNamespace("jaspBase")
get0 <- function(n) get(n, envir=ns)

outdir <- {
	args <- commandArgs(trailingOnly=TRUE)
	if (length(args) >= 1) args[[1]] else file.path(getwd(), "tests", "equivalence", "fixtures")
}
dir.create(outdir, showWarnings=FALSE, recursive=TRUE)

get0("setDeveloperMode")(FALSE)

results <- get0("create_cpp_jaspResults")("Golden Baseline", NULL)
get0("setResponseData")(42L, 7L)
results$setOptions('{"hypothesis":"twoSided","priorWidth":0.707,"pairs":[{"a":"x","b":"y"}]}')

# --- html: escaping, citations, messages -------------------------------------
h <- get0("create_cpp_jaspHtml")("Intro with <b>tags</b>, a < b & c > d.")
h$title <- "Introduction"
h$info  <- "some help text"
h$elementType <- "h2"
h$addCitation("JASP Team (2026). Golden baseline.")
h$addMessage("a message that only shows in convertToJSON")
results[["intro"]] <- h

# --- table: many column types, names, titles, footnotes ----------------------
t <- get0("create_cpp_jaspTable")("Descriptives")
t$setData(list(
	num  = c(1.5, 2.5, NA, 4.5),
	int  = c(1L, 2L, 3L, 4L),
	txt  = c("a < b", "x & y", NA, "plain"),
	lgl  = c(TRUE, FALSE, TRUE, NA)
))
t$setRowNames(list("r1", "r2", "r3", "r4"))
t$setColTitles(list("Numeric", "Integer", "Text", "Logical"))
t$setColTypes(list("number", "integer", "string", "logical"))
t$status <- "complete"
t$addFootnoteHelper("table-level note", NULL, NULL, NULL)
t$addFootnoteHelper("column note", "*", "num", NULL)
t$addFootnoteHelper("cell note", "#", "txt", "r2")
results[["desc"]] <- t

# --- nested container with positions + collapsed -----------------------------
cont <- get0("create_cpp_jaspContainer")("Assumption Checks")
cont$initCollapsed <- TRUE

h2 <- get0("create_cpp_jaspHtml")("normality looks fine")
h2$title <- "Normality"
h2$position <- 2L
cont[["norm"]] <- h2

h3 <- get0("create_cpp_jaspHtml")("variances are equal")
h3$title <- "Homogeneity"
h3$position <- 1L
cont[["homog"]] <- h3

results[["checks"]] <- cont

# --- plot --------------------------------------------------------------------
p <- get0("create_cpp_jaspPlot")("Boxplot")
p$width <- 480L
p$height <- 320L
p$aspectRatio <- 1.5
p$status <- "complete"
p$filePathPng <- "state/figures/plot_0.png"
results[["boxplot"]] <- p

# --- state -------------------------------------------------------------------
st <- get0("create_cpp_jaspState")("model cache")
st$object <- list(coef=1.23, se=0.1, nested=list(a=1L, b="two"))
results[["cache"]] <- st

# --- report ------------------------------------------------------------------
rep <- get0("create_cpp_jaspReport")("A warning worth reporting.", TRUE)
rep$title <- "Report"
results[["report"]] <- rep

# --- column (no column funcs registered -> deterministic stand-alone path) ---
col <- get0("create_cpp_jaspColumn")("computedCol", FALSE)
results[["col"]] <- col

# --- qmlSource ---------------------------------------------------------------
q <- get0("create_cpp_jaspQmlSource")("priorWidthSlider")
q$setValue(list(changed=TRUE, value=0.707))
results[["qml"]] <- q

# --- dependency that survives, and one that gets pruned by changeOptions -----
keep <- get0("create_cpp_jaspHtml")("depends on hypothesis")
keep$title <- "Kept"
keep$dependOnOptions(c("hypothesis"))
results[["kept"]] <- keep

# a dependent TABLE: when pruned, old-results merge runs letChildrenRun() which
# flips its status complete -> running, making the prune visibly observable.
ptable <- get0("create_cpp_jaspTable")("Pruned table")
ptable$setData(list(v = c(1.0, 2.0, 3.0)))
ptable$status <- "complete"
ptable$dependOnOptions(c("hypothesis"))
results[["prunedTable"]] <- ptable

responseJson <- results$getResults()

# --- option change -> dependency pruning + old-results merge -----------------
results$changeOptions('{"hypothesis":"less","priorWidth":0.707,"pairs":[{"a":"x","b":"y"}]}')
prunedJson <- results$getResults()

# --- saved-state path (convertToJSON) ---------------------------------------
tmpRoot <- tempfile("goldenSave")
dir.create(tmpRoot)
get0("setSaveLocation")(tmpRoot, "jaspResults.json")
results$saveResults()
savedJson <- paste(readLines(file.path(tmpRoot, "jaspResults.json"), warn=FALSE), collapse="\n")

writeLines(responseJson, file.path(outdir, "golden_response.json"), sep="")
writeLines(prunedJson,   file.path(outdir, "golden_pruned.json"),   sep="")
writeLines(savedJson,    file.path(outdir, "golden_saved.json"),    sep="")

cat("wrote golden_response.json (", nchar(responseJson), "), golden_pruned.json (",
	nchar(prunedJson), "), golden_saved.json (", nchar(savedJson), "bytes ) to", outdir, "\n")
