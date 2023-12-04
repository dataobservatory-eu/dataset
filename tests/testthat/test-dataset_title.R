myiris <- iris_dataset

test_that("dataset_title() works", {
  expect_true(is.character(dataset_title(myiris)))
  expect_equal(dataset_title(myiris), "Iris Dataset")
})

test_that("dataset_title() <- assignment works", {
  expect_true(is.character(dataset_title(myiris)))
  expect_warning(dataset_title(myiris) <-"New title")
  expect_equal(dataset_title(myiris, overwrite = TRUE) <-"The Famous Iris Dataset", "The Famous Iris Dataset")
})
