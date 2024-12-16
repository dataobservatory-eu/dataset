
test_that("subsetting works", {
  expect_equal(ncol(iris_dataset[, 1]), 1)
  expect_equal(nrow(iris_dataset[1,2]), 1)
  expect_equal(iris$Sepal.Length[1], as.numeric(iris_dataset[1,2]) )
})

test_that("new_my_tibble() works", {
  myiris <- new_my_tibble(x=iris)
  expect_error(new_my_tibble(2))
  expect_equal(class(new_my_tibble(iris)), c("dataset_df", "tbl_df", "tbl", "data.frame"))
  expect_output(print(provenance(myiris)), "<http://example.com/dataset#>")
})

test_that("is.dataset_df() works", {
  expect_true(is.dataset_df(iris_dataset))
  expect_false(is.dataset_df(mtcars))
})

test_that("rbind works", {
  iris_dataset1 <- iris_dataset
  iris_dataset2 <- iris_dataset
  expect_equal(nrow(rbind(iris_dataset1, iris_dataset2)), 300)
})

test_that("dataset_df() works", {
  expect_equal(is.dataset_df(dataset_df(mtcars)), TRUE)
  expect_false(is.dataset_df(mtcars))
  expect_output(print(dataset_df(mtcars)), "Unknown Dataset", ignore.case = FALSE)
})

test_that("print.dataset_df() works", {
  expect_output(print(iris_dataset), "Anderson E", ignore.case = FALSE)
})

test_that("as_dataset_df() works", {
  expect_equal(is.dataset_df(as_dataset_df(iris)), TRUE)
  expect_false(is.dataset_df(mtcars))
})

test_that("summary.dataset_df() works", {
  expect_output(summary(iris_dataset), "https://doi.org/10.5281/zenodo.10396807", ignore.case = FALSE)
  expect_output(summary(iris_dataset), "Length of the sepal in cm \\(centimeter\\)", ignore.case = FALSE)
})

test_that("names.dataset_df() works", {
  expect_output(print(names(iris_dataset)), "rowid", ignore.case = FALSE)
  expect_output(print(names(iris_dataset)), "Sepal.Length", ignore.case = FALSE)
  expect_length(names(iris_dataset), 6)
  expect_equal(names(iris_dataset)[1], "rowid")
})


