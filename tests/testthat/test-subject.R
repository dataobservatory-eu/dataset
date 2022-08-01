a <- data.frame()
subject(a) <- NULL

test_that("subject_create() can return NULL", {
  expect_equal(subject(a), NULL)
})


x <- data.frame( geo = c("AL", "MK"),
                 value = c(1,2))
my_subject <- subject_create (
  term = c("R (Computer program language)",
           "Questionnaires--Computer programs"),
  subjectScheme = rep("LC Subject Headings", 2),
  schemeURI = rep("http://id.loc.gov/authorities/subjects",2),
  valueURI = c("https://id.loc.gov/authorities/subjects/sh2002004407.html",
                 "http://id.worldcat.org/fast/1085693/")
  )


simple_subjects <- subject_create(term = c("hello", "hi"),
                                  subjectScheme = rep(NA_character_, 2),
                                  schemeURI = rep(NA_character_,2),
                                  valueURI = rep(NA_character_,2))

test_that("subject_create() works", {
  expect_equal(simple_subjects$term, c("hello", 'hi'))
  expect_equal(my_subject$schemeURI, rep("http://id.loc.gov/authorities/subjects", 2))
})


subject(x) <- my_subject

y <- data.frame()
subject(y) <- "R (Computer program language)"
subject(y) <- "Questionnaires--Computer programs"


test_that("subject() works", {
  expect_equal(subject(x)$term,  as.character(c("R (Computer program language)",
                                    "Questionnaires--Computer programs")))
  expect_equal(subject(y)$term, as.character(c("R (Computer program language)", "Questionnaires--Computer programs")))
})


