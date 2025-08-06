# These methods are related to the defined() class but
# tested in a separate test suite.

test_that("as.character() drops class and metadata from defined vector", {
  x <- defined(c("apple", "banana", "cherry"),
               label = "Fruit",
               unit = "kg",
               concept = "http://example.org/fruit"
  )
  result <- as.character(x)
  expect_type(result, "character")
  expect_false(inherits(result, "haven_labelled_defined"))
  expect_false("unit" %in% names(attributes(result)))
})

test_that("as_character() without preserve_attributes drops metadata", {
  x <- defined(c("red", "green", "blue"),
               label = "Color",
               unit = "rgb",
               concept = "http://example.org/color"
  )
  result <- as_character(x)
  expect_type(result, "character")
  expect_false("unit" %in% names(attributes(result)))
  expect_false(inherits(result, "haven_labelled_defined"))
})

test_that("as_character() with preserve_attributes keeps metadata", {
  x <- defined(c("yes", "no"),
               label = "Binary",
               unit = "boolean",
               concept = "http://example.org/binary",
               namespace = "http://example.org/ns"
  )
  result <- as_character(x, preserve_attributes = TRUE)
  expect_type(result, "character")
  expect_equal(attr(result, "unit"), "boolean")
  expect_equal(attr(result, "concept"), "http://example.org/binary")
  expect_equal(attr(result, "namespace"), "http://example.org/ns")
})

