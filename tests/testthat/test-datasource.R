
test_that("datasource() works", {
  expect_equal(datasource(iris_dataset), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})

datasource(x=iris_dataset) <- NULL

test_that("datasource() initializes :unas unassigned value", {
  expect_equal(datasource(iris_dataset), ":unas")
})

datasource(iris_dataset) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"

test_that("dataset_source() <- assignment works", {
  expect_equal(datasource(iris_dataset), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})
