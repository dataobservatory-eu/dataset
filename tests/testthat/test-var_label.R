test_that("var_label() works", {
  expect_equal(var_label(iris_dataset$Sepal.Length),
               "Length of the sepal in cm")
  expect_equal(class(var_label(iris_dataset, unlist = TRUE)), "character")
  test_df <- dataset_df(a = 1:2, b = 3:4)
  expect_equal(var_label(test_df, unlist = TRUE, null_action = "fill"),
               c(rowid = "rowid", a = "a", b = "b"))
})

test_that("var_label()<-  works", {
  iris_dataset_2 <- iris_dataset
  var_label(orange_df$circumference) <- "circumference (breast height)"
  var_label(iris_dataset_2$Sepal.Length) <- "Length of the sepal in centimeters"
  expect_equal(var_label(iris_dataset_2$Sepal.Length),
               "Length of the sepal in centimeters")
  expect_equal(var_label(orange_df$circumference),
               "circumference (breast height)")
})

test_that("var_label() throws error", {
  test_df <- dataset_df(a = 1:2, b = 3:4)
  expect_error(var_label(test_df) <- c("A", "B", "C", "E"))
})


test_that("var_label.dataset_df() works", {
  expect_equal(length(var_label(iris_dataset, unlist = TRUE)), 6)
  expect_true(is.character(var_label(iris_dataset, unlist = TRUE)))
})

test_that("var_label.dataset_df() works", {
  d <- defined(x = 1:2)
  var_label(d) <- "test"
  expect_equal(var_label(d), "test")
})

test_that("var_label<- works for both types", {
  d <- defined(1:3)
  var_label(d) <- "demo"
  expect_equal(var_label(d), "demo")

  df <- dataset_df(x = defined(1:3, label = "a"))
  var_label(df$x) <- "renamed"
  expect_equal(var_label(df$x), "renamed")
})
