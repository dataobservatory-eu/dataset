iris_dataset <- iris
rights(iris_dataset) <- "CC-BY-SA"


test_that("rights() works", {
  expect_equal(rights(iris_dataset), "CC-BY-SA")
})
