test_that("dataset_title() works", {
  expect_true(is.character(dataset_title(iris_dataset)))
  expect_equal(dataset_title(iris_dataset), "Iris Dataset")
})

test_that("set_dataset_title() works", {
  expect_true(is.character(dataset_title(iris_dataset)))
  expect_equal(dataset_title(set_dataset_title(iris_dataset, "The Famous Iris Dataset" )), "The Famous Iris Dataset")
})
