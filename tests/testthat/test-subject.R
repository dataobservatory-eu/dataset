


test_that("subject() <- works", {
  expect_message(subject(iris_dataset))
  iris_dataset_2 <- iris_dataset
  subject(iris_dataset_2) <- NULL
  expect_equal(subject(iris_dataset_2)$term, ":tba")
  expect_equal(subject(iris_dataset_2)$prefix, "")
  a <- new_Subject(term = c("test1", "test2"))
  expect_equal(new_Subject(term = c("test1", "test2"))$term, c("test1", "test2"))
})

