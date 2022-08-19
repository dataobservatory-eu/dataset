iris_dataset <- iris
identifier(iris_dataset) <- NULL

test_that("identifier() works", {
  expect_equal(identifier(iris_dataset), NA_character_)
  expect_equal(identifier(iris_dataset) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x", "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})
