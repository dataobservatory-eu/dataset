

test_that("geolocation() works", {
  iris_dataset_2 <- iris_dataset
  geolocation(iris_dataset_2) <- "US"
  expect_equal(geolocation(iris_dataset_2), "US")
  expect_message(geolocation(iris_dataset_2, overwrite=FALSE) <- "GB")
})

test_that("geolocation(x) <- NULL works", {
  iris_dataset_2 <- iris_dataset
  geolocation(iris_dataset_2) <- "US"
  geolocation(iris_dataset_2) <- NULL
  expect_equal(geolocation(iris_dataset_2), NULL)
})
