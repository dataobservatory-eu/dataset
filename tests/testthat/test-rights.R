iris_dataset <- iris
rights(iris_dataset) <- NULL

test_that("rights() works", {
  expect_equal(rights(iris_dataset), NA_character_)
  expect_equal(rights(iris_dataset) <- "CC-BY-SA", "CC-BY-SA")
  expect_message(rights(iris_dataset, FALSE) <- "CC0")
})
