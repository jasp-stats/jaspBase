library(jaspBase)

# Example 1: assign columns directy using createMixedColumn (using createMixedColumn is the recommended approach) ----
tb <- createJaspTable()

data <- createMixedColumn(
  values = list(1.23, 0.04, "hoi", 123),
  types =  c("number", "pvalue", "string", "integer")
)

tb[["col"]] <- data
tb[["col2"]] <- seq(length(data))
tb

tbR <- tb$toRObject()
all.equal(tbR$col0, data) # TRUE
all.equal(tbR$col1, seq(length(data))) # TRUE

# Example 2: use setData with createMixedColumn ----
tb <- createJaspTable()

df <- data.frame(
  col  = createMixedColumn(
      values = list(1.23, 0.04, "hoi", 123),
      types =  c("number", "pvalue", "string", "integer")
  ),
  col2 = 1:4
)


df
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

tbR <- tb$toRObject()
all.equal(tbR$col, df$col) # TRUE
all.equal(tbR$col2, df$col2) # TRUE
all.equal(tbR, df, check.attributes = FALSE) # TRUE


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
  name1  = createMixedColumn(
    values = list(1.23, 0.04, "hoi", 123),
    types =  c("number", "pvalue", "string", "integer")
  ),
  name2 = 1:4
)

rowList <- lapply(seq_len(nrow(df)), function(i) {
  list(
    name1 = createMixedRow(
      value = df$name1[[i]]$value,
      type  = df$name1[[i]]$type
    ),
    name2 = i
  )
})

# one by one
tb
tb$addRows(rows = rowList[[1]])
tb
tb$addRows(rows = rowList[[2]])
tb
tb$addRows(rows = rowList[[3]])
tb
tb$addRows(rows = rowList[[4]])
tb

# all at once
tb <- createJaspTable()
tb$addRows(rows = rowList)
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

# temporary for comparison with master
library(jaspBase)
tb1 <- createJaspTable()
tb1$addRows(rows = list(a = 1, b = 2, c = 3))
tb1
tb1$addRows(rows = list(a = 2, b = 4, c = 6))
tb1
tb1$addRows(rows = list(d = 3, f = 5))
tb1


rowList2 <- list(
  list(a = 1, b = 2),
  list(a = 2, b = 4),
  list(a = 3, b = 6),
  list(c = 3, d = 4),
  list(c = 6, d = 8)
)

tb2 <- createJaspTable()
tb2$addRows(rows = rowList2)
tb2

# this crashes!
tb1 <- createJaspTable()
for (l in letters[1:6])
  tb1$addColumnInfo(name = l, title = l, type = "integer")
tb1$addRows(rows = list(a = 1, b = 2, c = 3))
tb1
tb1$addRows(rows = list(a = 2, b = 4, c = 6))
tb1
tb1$addRows(rows = list(d = 3, e = 5))
tb1


rowList2 <- list(
  list(a = 1, b = 2),
  list(a = 2, b = 4),
  list(a = 3, b = 6),
  list(c = 3, d = 4),
  list(c = 6, d = 8)
)

tb2 <- createJaspTable()
tb2$addRows(rows = rowList2)
tb2
