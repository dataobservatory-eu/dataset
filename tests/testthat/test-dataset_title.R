test_that("dataset_title() works", {
  expect_equal(dataset_title(iris_dataset), "Iris Dataset")
  expect_error(dataset_title(mtcars))
  expect_error(dataset_title(mtcars) <- "Error")
})

test_that("dataset_title() <- value works with overwrite", {
  iris_dataset_2 <- iris_dataset
  dataset_title(x=iris_dataset_2, overwrite = TRUE) <-"The Famous Iris Dataset"
  expect_equal(dataset_title(iris_dataset_2), "The Famous Iris Dataset")
})

test_that("dataset_title() <- value works without overwrite", {
  iris_dataset_2 <- iris_dataset
  expect_warning(dataset_title(x=iris_dataset_2, overwrite = FALSE) <-"The Most Famous Iris Dataset")
})

