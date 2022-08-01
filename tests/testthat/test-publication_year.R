iris_dataset <- iris
publication_year(iris_dataset) <- 1935
publication_year(iris_dataset)

test_that("publication_year() works", {
  expect_equal(publication_year(iris_dataset), 1935)
})
