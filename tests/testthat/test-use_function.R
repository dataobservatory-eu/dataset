
dont_test <- function() {
  temp_path <- file.path(tempdir(), "iris.csv")
  write.csv(iris, file = temp_path, row.names = F)

  iris_dataset <- read_dataset(
    dataset_id = "iris_dataset",
    obs_id = NULL,
    dimension_names = NULL,
    measure_names = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    attribute_names = "Species",
    Title = "Iris Dataset",
    unit = list(code = "MM", label = "millimeter"),
    .f = "utils::read.csv",
    file = temp_path )
  attributes(iris_dataset)

  test_that("read_dataset works", {
    expect_equal(attr(iris_dataset, "Identifier"), "iris_dataset")
    expect_equal(attr(iris_dataset, "RelatedIdentifier")$relationType, "IsCompiledBy")
  })
}


