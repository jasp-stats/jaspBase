#!/usr/bin/env Rscript
# Dumps the complete RCPP_MODULE(jaspResults) surface of the loaded jaspBase:
# module functions, classes, methods, properties — names, signatures, docstrings.
#
# Used as a fingerprint to guarantee the R-visible API stays bit-identical
# across the core/adapters refactor (see tmp/plan-python-interface.md, Phase 1).
#
# Usage:
#   JASP_EQUIV_RLIB=/path/to/lib Rscript moduleFingerprint.R [outfile]
# (JASP_EQUIV_RLIB optional; prepended to .libPaths so a freshly built jaspBase
#  can be fingerprinted instead of the system-wide installation.)

if (Sys.getenv("JASP_EQUIV_RLIB") != "")
	.libPaths(c(Sys.getenv("JASP_EQUIV_RLIB"), .libPaths()))

suppressPackageStartupMessages(library(jaspBase))

out <- character()
emit <- function(...) out <<- c(out, paste0(...))

norm <- function(x)
{
	if (is.null(x) || length(x) == 0) return("")
	x <- paste(x, collapse=" | ")
	# demangle libc++/libstdc++ std::string so fixtures are toolchain-stable
	x <- gsub("std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char> >", "std::string", x, fixed=TRUE)
	x <- gsub("std::__1::basic_string<char, std::__1::char_traits<char>, std::__1::allocator<char>>", "std::string", x, fixed=TRUE)
	x <- gsub("std::__cxx11::basic_string<char, std::__cxx11::char_traits<char>, std::allocator<char> >", "std::string", x, fixed=TRUE)
	x <- gsub("std::__cxx11::basic_string<char, std::__cxx11::char_traits<char>, std::allocator<char>>", "std::string", x, fixed=TRUE)
	gsub("\\s+", " ", trimws(x))
}

envData <- function(obj)
{
	# C++OverloadedMethods / C++Field keep their data in a .xData environment
	if (isS4(obj) && ".xData" %in% slotNames(obj))
		return(slot(obj, ".xData"))
	NULL
}

xdGet <- function(env, name)
{
	if (!is.null(env) && exists(name, envir=env, inherits=FALSE)) get(name, envir=env) else NULL
}

ns <- asNamespace("jaspBase")
module <- get(".__Mod__jaspResults", envir=ns)
storage <- get("storage", envir=slot(module, ".xData"))

emit("# jaspResults RCPP_MODULE surface fingerprint")
emit("# jaspBase version: ", as.character(packageVersion("jaspBase")))
emit("")
emit("## module functions")

for (name in sort(ls(storage)))
{
	obj <- get(name, envir=storage)
	if (!inherits(obj, "C++Function")) next
	emit("FUN ", name)
	emit("  signature: ", norm(attr(obj, "signature")))
	emit("  docstring: ", norm(attr(obj, "docstring")))
}

emit("")
emit("## classes")

for (name in sort(ls(storage)))
{
	obj <- get(name, envir=storage)
	if (!inherits(obj, "C++Class")) next

	emit("CLASS ", name)
	emit("  docstring: ", norm(slot(obj, "docstring")))
	emit("  parents: ", paste(sort(slot(obj, "parents")), collapse=", "))

	fields <- slot(obj, "fields")
	for (fname in sort(names(fields)))
	{
		fenv <- envData(fields[[fname]])
		emit("  FIELD ", fname)
		emit("    cpp_class: ", norm(xdGet(fenv, "cpp_class")))
		emit("    docstring: ", norm(xdGet(fenv, "docstring")))
	}

	methods <- slot(obj, "methods")
	for (mname in sort(names(methods)))
	{
		menv <- envData(methods[[mname]])
		emit("  METHOD ", mname)
		emit("    nargs: ", norm(xdGet(menv, "nargs")))
		emit("    void: ",  norm(xdGet(menv, "void")))
		emit("    const: ", norm(xdGet(menv, "const")))
		for (sig in xdGet(menv, "signatures")) emit("    signature: ", norm(sig))
		for (doc in xdGet(menv, "docstrings")) emit("    docstring: ", norm(doc))
	}
}

result <- paste(out, collapse="\n")

args <- commandArgs(trailingOnly=TRUE)
if (length(args) >= 1)
{
	writeLines(result, args[[1]])
	cat("fingerprint written to", args[[1]], "(", length(out), "lines )\n")
} else
	cat(result, "\n")
