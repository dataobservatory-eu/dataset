iris_dataset <- iris
dataset_source(iris_dataset) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"

test_that("dataset_source() works", {
  expect_equal(dataset_source(iris_dataset), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})
