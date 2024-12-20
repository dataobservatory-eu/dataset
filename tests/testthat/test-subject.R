
test_that("subject_create() <- works", {
  default_subject <- subject_create(
    term = "data sets",
    subjectScheme = "Library of Congress Subject Headings (LCSH)",
    schemeURI = "https://id.loc.gov/authorities/subjects.html",
    valueURI = "http://id.loc.gov/authorities/subjects/sh2018002256")
  expect_equal(default_subject$term, "data sets")
  expect_true(is.subject(default_subject))
  expect_equal(subject_create(term=NULL)$term, ":tba")
})

test_that("subject_create() <- works", {
  tested_1 <- new_Subject(term="dataset",
                          subjectScheme = "LCCH",
                          schemeURI =  "http://id.loc.gov/authorities/subjects",
                          valueURI="https://id.loc.gov/authorities/subjects/sh85068079")
  expect_equal(tested_1$term, "dataset")
  expect_equal(tested_1$subjectScheme, "LCCH")
  expect_equal(tested_1$schemeURI, "http://id.loc.gov/authorities/subjects")
  expect_equal(tested_1$valueURI, "https://id.loc.gov/authorities/subjects/sh85068079")
  expect_equal(tested_1$classificationCode, NULL)
  tested_2 <- new_Subject(term="dataset",
                          classificationCode = "test")
  expect_equal(tested_2$classificationCode, "test")
  expect_error(subject(iris_dataset) <- 2)
})

test_that("subject() <- works", {
  iris_dataset_2 <- iris_dataset
  subject(x=iris_dataset_2) <- NULL
  expect_equal(subject(iris_dataset_2)$term, ":tba")
  expect_equal(subject(iris_dataset_2)$prefix, "")
  a <- new_Subject(term = c("test1", "test2"))
  expect_equal(new_Subject(term = c("test1", "test2"))$term, c("test1", "test2"))
  subject(x=iris_dataset_2) <- "iris"
  expect_equal(subject(iris_dataset_2)$term, "iris")
  expect_equal(subject(iris_dataset_2)$subjectScheme, "")
  expect_equal(subject(iris_dataset_2)$schemeURI, "")
  expect_equal(subject(iris_dataset_2)$valueURI, "")
})

subject(iris_dataset)




