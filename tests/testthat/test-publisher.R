test_that("publisher() works", {
  expect_equal(publisher(iris_dataset), "American Iris Society")
})


test_that("publisher() <- assignment works", {
  iris_dataset2 <- iris_dataset
  expect_equal(publisher(iris_dataset2), "American Iris Society")
  expect_message(publisher(iris_dataset2, overwrite=FALSE) <- "Overwritten")
  expect_equal(publisher(iris_dataset2, overwrite=TRUE) <- "Overwritten", "Overwritten")
})

test_that("publisher(x) <- c(1:2) throws and error", {
  expect_error(publisher(iris_dataset) <- c(1:2))
})
