test_that("in R the results are decoded through jaspSyntax", {
  local_mocked_bindings(
    jaspResultsCalledFromJasp = function() FALSE,
    .hasJaspSyntax            = function() TRUE,
    .decodeJsonText           = function(json) gsub("JASPColumn_D1_F1_0_Encoded", "contNormal", json, fixed = TRUE)
  )

  expect_identical(.decodedResults('{"title":"JASPColumn_D1_F1_0_Encoded"}'), '{"title":"contNormal"}')
})

test_that("in JASP the results are left encoded: the engine decodes what it sends", {
  local_mocked_bindings(
    jaspResultsCalledFromJasp = function() TRUE,
    .decodeJsonText           = function(json) stop("must not decode")
  )

  expect_identical(.decodedResults('{"title":"JASPColumn_D1_F1_0_Encoded"}'), '{"title":"JASPColumn_D1_F1_0_Encoded"}')
})

test_that("without jaspSyntax the results are given as they are", {
  local_mocked_bindings(
    jaspResultsCalledFromJasp = function() FALSE,
    .hasJaspSyntax            = function() FALSE,
    .decodeJsonText           = function(json) stop("must not decode")
  )

  expect_identical(.decodedResults('{"title":"x"}'), '{"title":"x"}')
})
