iris_dataset <- dataset(
  x = iris,
  title = "Iris Dataset",
  author = person("Edgar", "Anderson", role = "aut"),
  source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en",
  description = "This famous (Fisher's or Anderson's) iris data set."
)

test_that("publisher() works", {
  expect_true(is.null(publisher(iris_dataset)))
})

iris_dataset2 <- iris_dataset
publisher(iris_dataset2) <- "American Iris Society"

test_that("publisher() <- assignment works", {
  expect_equal(publisher(iris_dataset2), "American Iris Society")
  expect_message(publisher(iris_dataset2, overwrite=FALSE) <- "Overwritten")
})


publisher(iris_dataset) <- NULL

test_that("publisher() works", {
  expect_equal(publisher(iris_dataset), ":tba")
})

test_that("publisher(x) <- c(1:2) throws and error", {
  expect_error(publisher(iris_dataset) <- c(1:2))
})
