rights(iris_dataset) <- NULL
iris_dataset2 <- iris_dataset

value <- "CC-BY-SA"

rights(iris_dataset2) <- "CC-BY-SA"
rights(iris_dataset2)

test_that("rights() works", {
  expect_equal(rights(iris_dataset), ":unas")
  expect_error(rights(iris_dataset) <- c(":unas", "hello"))
  expect_equal(rights(iris_dataset2),  "CC-BY-SA")
  expect_message(rights(iris_dataset, FALSE) <- "CC0")
})
