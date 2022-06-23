dataset (  x = iris,
           dataset_id = "iris_dataset", obs_id = NULL,
           dimensions = NULL,
           measurements = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
           attributes = c("Species"),
           unit = list(code="MM", label = "milimeters")
           )


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
