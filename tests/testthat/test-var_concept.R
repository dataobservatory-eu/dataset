test_that("var_concept() works for defined vectors", {
  x <- defined(c(1, 2, 3), label = "A")
  expect_null(var_concept(x))

  var_concept(x) <- "http://example.org/concept/ABC"
  expect_equal(var_concept(x), "http://example.org/concept/ABC")

  var_concept(x) <- NULL
  expect_null(var_concept(x))
})

test_that("var_concept() errors for invalid inputs", {
  x <- defined(c(1, 2, 3), label = "A")
  expect_error(var_concept(x) <- c("uri1", "uri2"),
               "must be a single character string or NULL")
  expect_error(var_concept(x) <- 123,
               "must be a single character string or NULL")
  expect_error(var_concept(x) <- list("not a string"),
               "must be a single character string or NULL")
})

test_that("get_variable_concepts() returns named list of concepts", {
  df <- dataset_df(
    var1 = defined(c(1, 2), label = "V1", concept = "http://concept.org/v1"),
    var2 = defined(c(3, 4), label = "V2")
  )
  concepts <- get_variable_concepts(df)

  expect_type(concepts, "list")
  expect_named(concepts, c("rowid", "var1", "var2"))
  expect_equal(concepts$var1, "http://concept.org/v1")
  expect_null(concepts$var2)
})

test_that("get_variable_concepts() errors on non-dataset_df input", {
  expect_error(get_variable_concepts(data.frame(x = 1:3)),
               "must be a dataset_df object")
})
