
test_that("subject_create() <- works", {
  default_subject <- subject_create(
    term = "data sets",
    subjectScheme = "Library of Congress Subject Headings (LCSH)",
    schemeURI = "https://id.loc.gov/authorities/subjects.html",
    valueURI = "http://id.loc.gov/authorities/subjects/sh2018002256")
  expect_equal(default_subject$term, "data sets")
  expect_true(is.subject(default_subject))
})

test_that("subject() <- works", {
  iris_dataset_2 <- iris_dataset
  subject(x=iris_dataset_2) <- NULL
  expect_equal(subject(iris_dataset_2)$term, ":tba")
  expect_equal(subject(iris_dataset_2)$prefix, "")
  a <- new_Subject(term = c("test1", "test2"))
  expect_equal(new_Subject(term = c("test1", "test2"))$term, c("test1", "test2"))
})

