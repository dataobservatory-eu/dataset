test_that("var_labels() works::", {
  expect_error(var_labels(iris)) # must only work with a dataset object
})

relabelled <- set_var_labels(
  x=iris_dataset,
  value= c(Sepal.Length="The sepal length measured in cm.",
           Sepal.Width ="The sepal width measured in centimeters.",
           Species     ="The species of the iris observed.")
)

test_that("set_var_labels() works::", {
  expect_equal(as.character(var_labels(relabelled)["Sepal.Length"]), "The sepal length measured in cm.")
  expect_equal(as.character(var_labels(relabelled)["Petal.Width"]), "The petal width of iris specimen in centimeters.")
})
