dct_iris <- dublincore_add(
  x = iris,
  Title = "Iris Dataset",
  Creator = person("Edgar", "Anderson", role = "aut"),
  Publisher = "American Iris Society",
  Source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  Date = 1935,
  Language = "en"
)

y <- dataset (data.frame())
y <- dublincore_add(x=y, Creator = person("Jane", "Doe"))

test_that("dublincore works", {
  expect_equal(dublincore(dct_iris)$Language, 'eng')
  expect_equal(publisher(dct_iris), "American Iris Society")
  expect_equal(dataset_source(dct_iris), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
  expect_equal(dublincore(dct_iris)$Creator, person("Edgar", "Anderson", role = "aut"))
  expect_equal(creator(dct_iris), person("Edgar", "Anderson", role = "aut"))
  expect_equal(creator(y), person("Jane", "Doe"))
  expect_equal(resource_type(y)$resourceType, "DCMITYPE:Dataset")
})

my_iris_dataset <- dataset(
  x = iris,
  Dimensions = NULL,
  Measures = c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ),
  Attributes = "Species",
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

test_that("dublincore_add(creator) works", {
  expect_equal(creator(my_iris_dataset), person("Edgar", "Anderson", role = "aut"))
})

