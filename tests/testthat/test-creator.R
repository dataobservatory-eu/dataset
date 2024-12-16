test_that("creator() works", {
  expect_equal(creator(iris_dataset), person(given="Edgar", family="Anderson", role = "aut"))
})


test_that("creator() <- value works with overwrite", {
  iris_dataset_2 <- iris_dataset
  creator(iris_dataset_2) <- person(given="Jane", family="Doe")
  expect_equal(creator(iris_dataset_2), person(given="Jane", family="Doe"))
})



test_that("creator() <- value works without overwrite", {
  iris_dataset_3 <- iris_dataset
  creator(x=iris_dataset_3, overwrite=FALSE) <- person("Jane", "Doe")
  expect_equal(creator(iris_dataset_3),
               c(person(given="Edgar", family="Anderson", role = "aut"), person("Jane", "Doe")))
})


test_that("creator() <- throws error", {
  iris_dataset_4 <- iris_dataset
  expect_error(creator(x=iris_dataset_4, overwrite=TRUE) <- 12)
})

test_that("creator() <- NULL", {
  iris_dataset_5 <- iris_dataset
  creator(iris_dataset_5) <- NULL
  expect_equal(creator(iris_dataset_5), creator(iris_dataset))
})


