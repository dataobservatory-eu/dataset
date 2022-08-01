iris_dataset <- iris
creator(iris_dataset) <- person("Anderson", "Edgar", role = "aut")

test_that("creator() works", {
  expect_equal(creator(iris_dataset), person("Anderson", "Edgar", role = "aut"))
  expect_error(creator(x = data.frame()) <- "Edgar Anderson")
})
