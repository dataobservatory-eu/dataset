irissubject <- new_Subject (term  = "Irises (plants)",
                            schemeURI = "http://id.loc.gov/authorities/subjects",
                            valueURI = "https://id.loc.gov/authorities/subjects/sh85068079",
                            subjectScheme = "LCCH",
                            prefix = "lcch:")

test_that("new_Subject() works as a constructor", {
  expect_true(is.subject(x=irissubject))
  expect_equal(irissubject$prefix, "lcch:")
  expect_equal(irissubject$valueURI, "https://id.loc.gov/authorities/subjects/sh85068079")
})


myiris <- iris_dataset
subject(myiris) <- list  (term  = "Irises (plants)",
                          schemeURI = "http://id.loc.gov/authorities/subjects",
                          valueURI = "https://id.loc.gov/authorities/subjects/sh85068079",
                          subjectScheme = "LCCH",
                          prefix = "lcch:")

test_that("subject() works", {
  expect_true(is.subject(subject(myiris)))
  expect_equal(subject(myiris)$prefix, "lcch:")
  expect_equal(subject(myiris)$valueURI, "https://id.loc.gov/authorities/subjects/sh85068079")
})


subject(myiris) <- "Iris"

test_that("subject <- assignment works with a string", {
  expect_true(is.subject(subject(myiris)))
  expect_equal(subject(myiris)$prefix, "")
  expect_equal(subject(myiris)$term, "Iris")
})
