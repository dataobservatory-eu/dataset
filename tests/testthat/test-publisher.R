iris_dataset <- iris
publisher(iris_dataset) <- "American Iris Society"


test_that("publisher() works", {
  expect_equal(publisher(iris_dataset), "American Iris Society")
  expect_message(publisher(iris_dataset, overwrite=FALSE) <- "Overwritten")
})


publisher(iris_dataset) <- NULL

test_that("publisher() works", {
  expect_equal(publisher(iris_dataset), NA_character_)
})

test_that("publisher(x) <- c(1:2) throws and error", {
  expect_error(publisher(iris_dataset) <- c(1:2))
})
