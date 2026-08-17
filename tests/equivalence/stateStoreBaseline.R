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

results[["cache"]]  <- st
results[["cache2"]] <- st2

# --- harvest other objects for state ----------------------------------------
# keyed by envName (state_0, state_1, ... in construction order)
others <- results$getOtherObjectsForState()
emit("harvest names: ", paste(sort(names(others)), collapse=","))
emit("harvest length: ", length(others))

# state_0 was created first (val), state_1 second (vec)
emit("harvest state_0 identical to val: ", identical(others[["state_0"]], val))
emit("harvest state_1 identical to vec: ", identical(others[["state_1"]], vec))

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
