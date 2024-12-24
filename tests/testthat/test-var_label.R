
test_that("var_label() works", {
  expect_equal(var_label(iris_dataset$Sepal.Length), "Length of the sepal in cm")
  expect_equal(class(var_label(iris_dataset, unlist=TRUE)), "character")
  test_df <- dataset_df(a = 1:2, b=3:4)
  expect_equal(var_label(test_df, unlist=TRUE, null_action = "fill" ), c(rowid="rowid", a = "a", b="b"))
  #expect_equal(label_attribute(iris_dataset$Species), "Taxon name within the Iris genus")
})

test_that("var_label()<-  works", {
  iris_dataset_2 <- iris_dataset
  var_label(iris_dataset_2$Sepal.Length) <- "Length of the sepal in centimeters"
  expect_equal(var_label(iris_dataset_2$Sepal.Length), "Length of the sepal in centimeters")
})



test_that("var_label() throws error", {
  test_df <- dataset_df(a = 1:2, b=3:4)
  expect_error(var_label(test_df) <- c("A", "B", "C", "E"))
})


test_that("var_label.dataset_df() works", {
  expect_equal(length(var_label(iris_dataset, unlist=TRUE)), 6)
  expect_true(is.character(var_label(iris_dataset, unlist=TRUE)))
})

test_that("var_label.dataset_df() works", {
  d <- defined(x=1:2)
  var_label(d) <- "test"
  expect_equal(var_label(d), "test")
})

dataset_df(mtcars, identifier = c(mt="http://mtcars.com/"))
