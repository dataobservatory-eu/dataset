test_that("description() <- assignment works", {
  iris_dataset_2 <- iris_dataset
  expect_equal(description(iris_dataset_2), "The famous (Fisher's or Anderson's) iris data set.")
  expect_warning(description(iris_dataset_2, overwrite=F) <- "Overwritten.")
})


test_that("description() works", {
  iris_dataset_2 <- iris_dataset
  description(iris_dataset_2, TRUE) <- "Overwritten."
  expect_equal(description(iris_dataset_2), "Overwritten.")
})


test_that("description() works", {
  iris_dataset_2 <- iris_dataset
  description(x=iris_dataset_2, overwrite=TRUE) <- NULL
  expect_equal(description(iris_dataset_2), ":unas")
})
