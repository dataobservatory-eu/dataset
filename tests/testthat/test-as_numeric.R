# These methods are related to the defined() class but
# tested in a separate test suite.

test_that("as_numeric() drops metadata by default", {
  gdp <- defined(c(3897L, 7365L), label = "GDP", unit = "million dollars")
  num <- as_numeric(gdp)
  expect_type(num, "integer")
  expect_null(attr(num, "label"))
  expect_null(attr(num, "unit"))
  expect_false(inherits(num, "defined"))
})

test_that("as_numeric() preserves metadata with preserve_attributes = TRUE", {
  gdp <- defined(c(1000L, 2000L), label = "GDP", unit = "million dollars", concept = "http://example.org/GDP")
  num <- as_numeric(gdp, preserve_attributes = TRUE)
  expect_equal(attr(num, "unit"), "million dollars")
  expect_equal(attr(num, "concept"), "http://example.org/GDP")
  expect_type(num, "integer")
  expect_false("defined" %in% class(num))
})

test_that("as.numeric() drops all metadata (base R method)", {
  gdp <- defined(c(1, 2, 3), label = "GDP", unit = "million USD")
  rawnum <- as.numeric(gdp)
  expect_equal(rawnum, c(1, 2, 3))
  expect_null(attr(rawnum, "unit"))
  expect_type(rawnum, "double")
})

test_that("as_numeric() errors on character defined vectors", {
  x <- defined(c("a", "b", "c"), label = "Letters")
  expect_error(as_numeric(x), "underlying data is not numeric")
})
