iris_dataset <- iris
geolocation(iris_dataset) <- "US"

test_that("geolocation() works", {
  expect_equal(geolocation(iris_dataset), "US")
})

test_that("geolocation(..., overwrite=FALSE) gives a message", {
  expect_message(geolocation(iris_dataset, overwrite=FALSE) <- "GB")
})

geolocation(iris_dataset) <- NULL

test_that("geolocation(x) <- NULL works", {
  expect_equal(geolocation(iris_dataset), NULL)
})
