publication_year(iris_dataset) <- 1935

publication_year(iris_dataset)

test_that("publication_year() works", {
  expect_equal(publication_year(iris_dataset), as.character(1935))
  expect_warning(publication_year(iris_dataset, overwrite=F) <- 1934)
  })
publication_year(iris_dataset, overwrite=T) <- 1936

test_that("publication_year()<- assignment works", {
  expect_equal(publication_year(iris_dataset), as.character(1936))
})

publication_year(iris_dataset, overwrite=T) <- NULL

test_that("publication_year()<- NULL results in :unas", {
expect_equal(publication_year(x=iris_dataset), ":unas")
})
