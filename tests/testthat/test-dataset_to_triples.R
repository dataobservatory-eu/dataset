

test_that("dataset_to_triples works()", {
  expect_equal(class(dataset_to_triples(iris_dataset)), "data.frame")
  expect_equal(ncol(dataset_to_triples(iris_dataset)), 3)
  expect_equal(nrow(dataset_to_triples(head(iris_dataset, 3))), dim(head(iris_dataset, 3))[1]*dim(head(iris_dataset, 3))[2])
})






