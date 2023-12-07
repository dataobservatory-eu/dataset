identifier(iris_dataset) <- NULL

identifier(iris_dataset)

test_that("identifier() works", {
  expect_equal(identifier(iris_dataset), ":unas")
  })

identifier(iris_dataset) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"

test_that("identifier()<- assignment works", {
  expect_equal(identifier(iris_dataset), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})

