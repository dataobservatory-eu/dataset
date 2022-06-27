temp_path <- file.path(tempdir(), "iris.csv")
write.csv(iris, file = temp_path, row.names = F)
iris_ds <- read_dataset(
  dataset_id = "iris_dataset",
  obs_id = NULL,
  dimensions = NULL,
  measurements = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  attributes = "Species",
  Title = "Iris Dataset",
  unit = list(code = "MM", label = "millimeter"),
  .f = "utils::read.csv", file = temp_path )
attributes(iris_ds)

test_that("read_dataset works", {
  expect_equal(attr(iris_ds, "RelatedIdentifier")$relationType, "IsCompiledBy")
})

sessionInfo()
