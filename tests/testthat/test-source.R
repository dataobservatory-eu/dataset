iris_dataset <- iris

dataset_source(iris_dataset) <- NULL

test_that("dataset_source() initializes NA_character_", {
  expect_equal(dataset_source(iris_dataset), NA_character_)
})

dataset_source(iris_dataset) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"

test_that("dataset_source() works", {
  expect_equal(dataset_source(iris_dataset), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})


