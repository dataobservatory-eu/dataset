irissubject <- new_Subject (term  = "Irises (plants)",
                        schemeURI = "http://id.loc.gov/authorities/subjects",
                        valueURI = "https://id.loc.gov/authorities/subjects/sh85068079",
                        subjectScheme = "LCCH",
                        prefix = "lcch:")

test_that("subject() works as a constructor", {
  expect_true(is.subject(x=irissubject))
  expect_equal(irissubject$prefix, "lcch:")
  expect_equal(irissubject$valueURI, "https://id.loc.gov/authorities/subjects/sh85068079")
})


ds <- dataset(iris,
              title = "The iris Dataset",
              author = c(
                person(family ="Anderson",
                       given ="Edgar",
                       role = "aut")
              ),
              identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
              year = "1935",
              version = "1.0",
              description = "The famous dataset that is distributed with R.",
              url = "https://en.wikipedia.org/wiki/Iris_flower_data_set",
              subject = Subject (term  = "Dataset",
                                 subjectScheme = "LCCH",
                                 schemeURI = "http://id.loc.gov/authorities/subjects",
                                 valueURI = "https://id.loc.gov/authorities/subjects/sh2018002256",
                                 prefix = "lcch:")
)


test_that("subject() works", {
  expect_true(is.subject(subject(ds)))
  expect_equal(subject(ds)$prefix, "lcch:")
  expect_equal(subject(ds)$valueURI, "https://id.loc.gov/authorities/subjects/sh2018002256")
})


subject(ds) <- irissubject

test_that("subject <- assignment works", {
  expect_true(is.subject(subject(ds)))
  expect_equal(subject(ds)$prefix, "lcch:")
  expect_equal(subject(ds)$valueURI, "https://id.loc.gov/authorities/subjects/sh85068079")
})
