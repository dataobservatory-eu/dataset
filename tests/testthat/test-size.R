
size(iris_dataset) <- "estimate"

test_that("size()<- assignment works", {
  expect_true(grepl("KiB", size(iris_dataset)))
})

size(iris_dataset) <- "27.62 kB [26.97 KiB]"

test_that("size() works", {
  expect_equal("27.62 kB [26.97 KiB]", size(iris_dataset))
})
