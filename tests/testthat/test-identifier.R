


test_that("identifier() works", {
  iris_dataset_2 <- iris_dataset
  identifier(iris_dataset_2) <- NULL
  expect_equal(identifier(iris_dataset_2), ":unas")
})



test_that("identifier()<- assignment works", {
  iris_dataset_2 <- iris_dataset
  identifier(iris_dataset_2) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
  expect_equal(identifier(iris_dataset_2), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})


