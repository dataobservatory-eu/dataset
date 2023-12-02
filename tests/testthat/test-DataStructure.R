
test_that("initialise_dsd() works", {
  expect_equal(initialise_dsd(iris, 1)$Sepal.Length$name, "Sepal.Length")
})

dsd_iris <- DataStructure(iris_dataset)

test_that("DataStructure() works", {
  expect_equal(names(iris_dataset), names(dsd_iris))
  expect_equal(dsd_iris$Sepal.Length$range, "xsd:decimal")
})


dsd_iris$Sepal.Length$label <- "The sepal length measured in centimeters."
dsd_iris$Sepal.Width$label  <- "The sepal width measured in centimeters."
dsd_iris$Petal.Length$label <- "The petal length measured in centimeters."
dsd_iris$Petal.Width$label  <- "The petal width measured in centimeters."
dsd_iris$Species$label      <- "The name of the Iris species in the observation."


iris_dataset_labelled <- DataStructure_update(iris_dataset, dsd_iris)

test_that("DataStructure_update() works", {
  expect_equal(DataStructure(iris_dataset_labelled)$Sepal.Length$label,
               "The sepal length measured in centimeters.")
})



