iris_ds <- as.datacube ( x = iris[1:6,],
                         obs_id = NULL,
                         dim_names = NULL,
                         measure_names = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                         attribute_names = "Species")

iris_dataset <- dataset(iris_ds)
test_that("dataset", {
  expect_equal(attr(iris_dataset, "Title"), "Untitled Dataset")
  expect_equal(attr(iris_dataset, "Creator")$given, "unknown creator")
  expect_equal(attr(iris_dataset, "Publisher"), "<not yet published>")
  expect_equal(attr(iris_dataset, "PublicationYear"), as.integer(substr(as.character(Sys.Date()),1,4)
))
})
