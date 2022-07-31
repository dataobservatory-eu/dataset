my_iris_dataset <- dataset(
  x = iris,
  dimensions = NULL,
  measures = c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ),
  attributes = "Species",
  Title = "Iris Dataset"
)

my_iris_dataset <- dublincore_add(
  x = my_iris_dataset,
  Creator = person("Edgar", "Anderson", role = "aut"),
  Publisher = "American Iris Society",
  Source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  Date = 1935,
  Language = "en"
)

dataset_export(my_iris_dataset, file.path(tempdir(), "my_iris.csv"))

test_that("dataset_export", {
  expect_true(file.exists(file.path(tempdir(), "my_iris.csv")))
})
