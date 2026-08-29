#!/usr/bin/env Rscript
# Structural fingerprint of the toRObject() conversion tree.
#
# The core/adapters refactor replaces jaspObject's virtual toRObject() with a
# type-dispatch (rcppToRObject). This fingerprint locks down the R-side object
# conversion so a later regression is caught: for each object it records the
# S3 classes, names, and (for data.frames) column names/types, plus scalar
# fields that analyses rely on. It deliberately ignores environments and raw
# plot objects (not stable to compare).
#
# Usage:
#   JASP_EQUIV_RLIB=/path/to/lib Rscript toRObjectBaseline.R [outfile]

if (Sys.getenv("JASP_EQUIV_RLIB") != "")
	.libPaths(c(Sys.getenv("JASP_EQUIV_RLIB"), .libPaths()))

suppressPackageStartupMessages(library(jaspBase))

ns <- asNamespace("jaspBase")
get0 <- function(n) get(n, envir=ns)

get0("setDeveloperMode")(FALSE)

results <- get0("create_cpp_jaspResults")("toRObject baseline", NULL)
get0("setResponseData")(42L, 7L)
results$setOptions('{"hypothesis":"twoSided"}')

h <- get0("create_cpp_jaspHtml")("Intro with <b>tags</b>.")
h$title <- "Introduction"
results[["intro"]] <- h

t <- get0("create_cpp_jaspTable")("Descriptives")
t$setData(list(
	num = c(1.5, 2.5, NA),
	int = c(1L, 2L, 3L),
	txt = c("a", "b", "c"),
	lgl = c(TRUE, FALSE, TRUE)
))
t$setRowNames(list("r1", "r2", "r3"))
t$status <- "complete"
results[["desc"]] <- t

cont <- get0("create_cpp_jaspContainer")("Inner")
h2 <- get0("create_cpp_jaspHtml")("nested")
h2$title <- "Nested"
cont[["nested"]] <- h2
results[["inner"]] <- cont

rep <- get0("create_cpp_jaspReport")("a report", TRUE)
results[["report"]] <- rep

# --- fingerprinting ----------------------------------------------------------
out <- character()
emit <- function(...) out <<- c(out, paste0(...))

describeValue <- function(v, indent)
{
	pad <- strrep("  ", indent)
	cls <- paste(class(v), collapse=",")

	if (is.data.frame(v))
	{
		emit(pad, "class=", cls, " nrow=", nrow(v), " ncol=", ncol(v))
		cols <- character()
		for (cn in names(v))
			cols <- c(cols, paste0(cn, ":", paste(class(v[[cn]]), collapse="/")))
		emit(pad, "  columns: ", paste(cols, collapse=" | "))
		if (!is.null(attr(v, "footnotes"))) emit(pad, "  has footnotes attr")
		if (!is.null(attr(v, "title"))) emit(pad, "  title=", attr(v, "title"))
	}
	else if (is.list(v))
	{
		emit(pad, "class=", cls, " length=", length(v),
			 if (!is.null(names(v))) paste0(" names=", paste(names(v), collapse=",")) else "")
		if (!is.null(attr(v, "title"))) emit(pad, "  title=", attr(v, "title"))
		for (nm in names(v))
		{
			emit(pad, "  [", nm, "]")
			describeValue(v[[nm]], indent+2)
		}
	}
	else if (is.atomic(v) && length(v) <= 8)
	{
		emit(pad, "class=", cls, " values=", paste(format(v), collapse=","))
	}
	else
	{
		emit(pad, "class=", cls, " length=", length(v))
	}
}

walk <- function(obj, label)
{
	emit("OBJECT ", label, " (type=", obj$type, ")")
	r <- obj$toRObject()
	describeValue(r, 1)
	emit("")
}

walk(results[["intro"]],  "intro")
walk(results[["desc"]],   "desc")
walk(results[["inner"]],  "inner")
walk(results[["report"]], "report")
walk(results,             "results")

result <- paste(out, collapse="\n")

args <- commandArgs(trailingOnly=TRUE)
if (length(args) >= 1)
{
	writeLines(result, args[[1]])
	cat("toRObject fingerprint written to", args[[1]], "(", length(out), "lines )\n")
} else
	cat(result, "\n")
