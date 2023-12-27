
test_that("datasource_get() works", {
  expect_equal(datasource_get(iris_dataset), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})

test_that("datasource_set() initializes :unas unassigned value", {
  expect_equal(datasource_get( datasource_set(x=iris_dataset, value =  NULL, overwrite=TRUE)), ":unas")
})

datasource_set(iris_dataset, "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")

test_that("datasource_set() works", {
  expect_equal(datasource_get(iris_dataset), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})
