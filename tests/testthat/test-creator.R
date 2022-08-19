
test_that("creator() works", {
  expect_error(creator(iris) <- "hello")
  expect_equal(creator(iris) <- person("Anderson", "Edgar"), person("Anderson", "Edgar"))
  expect_message(creator(iris, overwrite=FALSE) <- person("Jane", "Doe"))
})

