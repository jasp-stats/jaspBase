library(jaspBase)

# Example 1.a: assign columns directy using convenience constructor (recommended approach) ----
tb <- createJaspTable()

data <- createMixedColumn(
  values = list(1.23, 0.04, "hoi", 123),
  types =  c("number", "pvalue", "string", "integer")
)

tb[["col"]] <- data
tb[["col2"]] <- seq(length(data))
tb

# Example 1.b: assign columns directy (not recommended) ---
tb <- createJaspTable()

data <- list(
  # | value | type      | format
  list(1.23,   "number",  "sf:4;dp:3"),
  list(0.04, "number",  "dp:3;p:.001"),
  list("hoi",  "string",  ""),
  list(123,    "integer", ""))
class(data) <- "mixed"

tb[["col"]] <- data
tb[["col2"]] <- seq(length(data))
tb

# Example 2a, setData with helper ----
tb <- createJaspTable()

df <- data.frame(
  col  = createMixedColumn(
      values = list(1.23, 0.04, "hoi", 123),
      types =  c("number", "pvalue", "string", "integer")
  ),
  col2 = 1:4
)


df$col
tb$setData(df)
tb

# Example 3a, addColumns pass data.frame ----
tb <- createJaspTable()

df <- data.frame(
  col  = createMixedColumn(
    values = list(1.23, 0.04, "hoi", 123),
    types =  c("number", "pvalue", "string", "integer")
  ),
  col2 = 1:4
)


tb$addColumns(cols = df)
tb

# Example 3b, addColumns pass list ----
tb <- createJaspTable()

lst <- list(
  col  = createMixedColumn(
    values = list(1.23, 0.04, "hoi", 123),
    types =  c("number", "pvalue", "string", "integer")
  ),
  col2 = 1:4
)


tb$addColumns(cols = lst)
tb

# Example 4a, addRows pass data.frame ----
tb <- createJaspTable()

df <- data.frame(
  col  = createMixedColumn(
    values = list(1.23, 0.04, "hoi", 123),
    types =  c("number", "pvalue", "string", "integer")
  ),
  col2 = 1:4
)


rowList <- lapply(seq_len(nrow(df)), function(i) {
  list(
    col = createMixedRow(
      value = df$col[[i]]$value,
      type  = df$col[[i]]$type
    ),
    col2 = i
  )
})

tb
tb$addRows(rows = rowList[1])
tb
tb$addRows(rows = rowList[2])
tb
tb$addRows(rows = rowList[3])
tb
tb$addRows(rows = rowList[4])
tb

tb <- createJaspTable()
tb$addRows(rows = rowList)
tb


# This should work, but something else goes wrong. We're looping over the elements of col, instead of over
# col and col2
tb <- createJaspTable()
tb$addRows(rows = rowList[[1]])
tb$addRows(rows = rowList[[2]])
tb$addRows(rows = rowList[[3]])
tb$addRows(rows = rowList[[4]])
tb


tb <- createJaspTable()
tb$addRows(rows = list(
  col = structure(list(value = 1.23, type = "number", format = "sf:4;dp:3"), class = c("mixed", "row")),
  col2 = 1L
))
tb

# Example 5a, addColumnInfo pass data.frame ----
tb <- createJaspTable()
tb$addColumnInfo(name = "m", title = "mixed Column", type = "mixed")
tb$addColumnInfo(name = "i", title = "int",           type = "integer")

data <- createMixedColumn(
  values = list(1.23, 0.04, "hoi", 123),
  types  = c("number", "pvalue", "string", "integer")
)

tb[["m"]] <- data
tb[["i"]] <- seq(length(data))
tb


jaspResultsCPP        <- jaspBase:::loadJaspResults("a name")
jaspResultsCPP$title  <- "a title"
jaspResults           <- jaspBase:::jaspResultsR$new(jaspResultsCPP)

jaspResults[["table"]] <- tb



returnThis <- jaspResultsCPP$getResults()
structure <- jsonlite::fromJSON(returnThis)
structure$results$table$data



tb <- createJaspTable()

data <- createMixedColumn(
  values = list(1.23, 0.04, "hoi", 123),
  types  = c("number", "pvalue", "string", "integer")
)

tb[["m"]] <- data
tb[["i"]] <- seq(length(data))
tb


jaspResultsCPP        <- jaspBase:::loadJaspResults("a name")
jaspResultsCPP$title  <- "a title"
jaspResults           <- jaspBase:::jaspResultsR$new(jaspResultsCPP)

jaspResults[["table"]] <- tb

jaspResultsCPP$send()

returnThis <- jaspResultsCPP$getResults()
structure <- jsonlite::fromJSON(returnThis)
structure$results$table$data
structure$results$table$schema




tb <- createJaspTable(title = "Mixed Table Test")

df <- data.frame(
  col  = createMixedColumn(
    values = list(1.23, 0.04, "hoi", 123),
    types  = c("number", "pvalue", "string", "integer")
  ),
  col2 = 1:4
)

tb$setData(df)
print(tb)

jaspResultsCPP        <- jaspBase:::loadJaspResults("a name")
jaspResultsCPP$title  <- "a title"
jaspResults           <- jaspBase:::jaspResultsR$new(jaspResultsCPP)


jaspResults[["mixedTestTableTest"]] <- tb


returnThis <- jaspResultsCPP$getResults()
structure <- jsonlite::fromJSON(returnThis)
structure$results$table$data
structure$results$table$schema

