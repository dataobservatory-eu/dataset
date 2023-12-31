
test_that("head() method works:", {
  expect_equal(nrow(head(iris_dataset,3)), 3)
})

test_that("tail() method works:", {
  expect_equal(nrow(tail(iris_dataset)), 6)
  expect_equal(creator(iris_dataset), creator(iris_dataset))
  expect_equal(dataset_title(tail(iris_dataset)), "Iris Dataset [subset of last observations]")
})

