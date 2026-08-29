#!/usr/bin/env Rscript
# Object-store round-trip + harvest baseline.
#
# Covers the path the JSON goldens cannot: jaspState objects stored in the
# jaspResults object store (Rcpp::Environment _RStorageEnv), retrieved, and
# harvested by getOtherObjectsForState(). Emits a deterministic text summary
# (identical() results + harvest names) that is byte-compared by runGate.sh.
#
# This is the safety net for migrating the store onto jaspHost (Phase 1, commit
# "wire store + jaspState").
#
# Usage:
#   JASP_EQUIV_RLIB=/path/to/lib Rscript stateStoreBaseline.R [outfile]

if (Sys.getenv("JASP_EQUIV_RLIB") != "")
	.libPaths(c(Sys.getenv("JASP_EQUIV_RLIB"), .libPaths()))

suppressPackageStartupMessages(library(jaspBase))

ns <- asNamespace("jaspBase")
get0 <- function(n) get(n, envir=ns)

out <- character()
emit <- function(...) out <<- c(out, paste0(...))

get0("setDeveloperMode")(FALSE)

# --- store access before any jaspResults exists ------------------------------
# Used to dereference a null storage env (R build) / silently vanish. With the
# lazy store + jaspHost default store this must just work and round-trip.
pre <- get0("create_cpp_jaspState")("pre results")
preVal <- list(early=TRUE, n=1L)
pre$object <- preVal
emit("pre-results state roundtrip identical: ", identical(pre$object, preVal))

# --- fresh plot defaults (no render pass yet) --------------------------------
pl <- get0("create_cpp_jaspPlot")("fresh dims")
emit("fresh plot dims: ", pl$width, "x", pl$height, " aspectRatio=", pl$aspectRatio)

results <- get0("create_cpp_jaspResults")("store baseline", NULL)
get0("setResponseData")(1L, 0L)
results$setOptions('{"a":1}')

# --- state round-trip --------------------------------------------------------
st <- get0("create_cpp_jaspState")("cache")
val <- list(coef=1.23, n=42L, tags=c("a","b"), nested=list(x=TRUE))
st$object <- val
got <- st$object
emit("state roundtrip identical: ", identical(val, got))

st2 <- get0("create_cpp_jaspState")("cache2")
vec <- c(1.5, 2.5, 3.5)
st2$object <- vec

# The "object stored: yes/no" text of jaspState::dataToString goes through
# jaspPrint straight to the process stdout (not capturable inside R), so probe
# it in a subprocess.
rlib <- Sys.getenv("JASP_EQUIV_RLIB")
probe <- sprintf(paste0(
	'.libPaths(c("%s", .libPaths())); suppressPackageStartupMessages(library(jaspBase));',
	'ns <- asNamespace("jaspBase"); get0 <- function(n) get(n, envir=ns);',
	'get0("setDeveloperMode")(FALSE);',
	'res <- get0("create_cpp_jaspResults")("probe", NULL);',
	'st <- get0("create_cpp_jaspState")("storedProbe"); st$object <- 42; st$print();',
	'get0("create_cpp_jaspState")("emptyProbe")$print()'), rlib)
printed <- system2("Rscript", c("-e", shQuote(probe)), stdout=TRUE, stderr=TRUE)
emit("state print says object stored yes: ", any(grepl("object stored: yes", printed, fixed=TRUE)))
emit("empty state print says object stored no: ", any(grepl("object stored: no", printed, fixed=TRUE)))

results[["cache"]]  <- st
results[["cache2"]] <- st2

# --- harvest other objects for state ----------------------------------------
# keyed by envName (state_0 is taken by the pre-results object above, so the
# two states here get state_1/state_2 in construction order)
others <- results$getOtherObjectsForState()
emit("harvest names: ", paste(sort(names(others)), collapse=","))
emit("harvest length: ", length(others))

# st was created first (val), st2 second (vec)
emit("harvest state_1 identical to val: ", identical(others[["state_1"]], val))
emit("harvest state_2 identical to vec: ", identical(others[["state_2"]], vec))

# --- keep list ---------------------------------------------------------------
keep <- results$getKeepList()
emit("keep length: ", length(keep))

result <- paste(out, collapse="\n")

args <- commandArgs(trailingOnly=TRUE)
if (length(args) >= 1)
{
	writeLines(result, args[[1]])
	cat("state-store baseline written to", args[[1]], "\n")
} else
	cat(result, "\n")
