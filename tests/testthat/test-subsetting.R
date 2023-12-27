test_that("Subsetting with [ single columns works:", {
  expect_equal(class(iris_dataset[, "Species"]), c("dataset", "data.frame"))
  expect_equal(dataset_title(iris_dataset[, "Species"]), paste0(dataset_title(iris_dataset), " [subset]"))
})

test_that("Subsetting with [ works:", {
  expect_equal(class(iris_dataset[1:2, 2:3]), c("dataset", "data.frame"))
  expect_equal(dim(iris_dataset[1:2, 2:3]), c(2,2))
  expect_equal(names(iris_dataset[1:2, 2:3]), c("Sepal.Width" ,"Petal.Length"))
  expect_equal(ncol(iris_dataset[1:2, 2:3]),2)
  expect_equal(nrow(iris_dataset[c(1, 6, 100:101), 2:3]),4)
  expect_equal(dataset_title(iris_dataset[1:2, 2:3]), paste0(dataset_title(iris_dataset), " [subset]"))
})

test_that("Subsetting with $ works:", {
  expect_equal(class(iris_dataset$Species), "factor")
  expect_equal(length(iris_dataset$Species), nrow(iris_dataset))
})

test_that("Subsetting with head() works:", {
  expect_equal(nrow(head(iris_dataset)),6)
  expect_true(is.dataset(head(iris_dataset)))
})


test_that("Subsetting with [[ works:", {
  expect_equal(iris_dataset[[1,2]], iris[[1,2]])
  expect_equal(iris_dataset[[2]], iris[[2]])
  expect_error(iris_dataset[[]])
  expect_error(iris_dataset[[6]])
})


