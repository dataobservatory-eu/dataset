test_that("subject_create() creates a valid subject object", {
  s <- subject_create(
    term = "Climate Data",
    subjectScheme = "LCSH",
    schemeURI = "http://id.loc.gov/authorities/subjects",
    valueURI = "https://id.loc.gov/authorities/subjects/sh2001000174",
    prefix = "lcsh:"
  )

  expect_s3_class(s, "subject")
  expect_equal(s$term, "Climate Data")
  expect_equal(s$subjectScheme, "LCSH")
  expect_equal(s$schemeURI, "http://id.loc.gov/authorities/subjects")
  expect_equal(s$valueURI, "https://id.loc.gov/authorities/subjects/sh2001000174")
  expect_equal(s$prefix, "lcsh:")
})

test_that("subject_create() handles NULL values gracefully", {
  s <- subject_create(term = NULL)
  expect_s3_class(s, "subject")
  expect_equal(s$term, ":tba")
})

test_that("subject_create() handles multiple terms", {
  terms <- c("Climate", "Agriculture")
  schemes <- c("LCSH", "LCSH")
  s <- subject_create(term = terms, subjectScheme = schemes)
  expect_type(s, "list")
  expect_s3_class(s[[1]], "subject")
  expect_equal(s[[1]]$term, "Climate")
  expect_equal(s[[2]]$term, "Agriculture")
})

test_that("new_Subject() constructs subject with and
          without classificationCode", {
  s1 <- new_Subject("Environment")
  expect_s3_class(s1, "subject")
  expect_equal(s1$term, "Environment")

  s2 <- new_Subject("Biology", classificationCode = "06")
  expect_s3_class(s2, "subject")
  expect_equal(s2$classificationCode, "06")
})

test_that("is.subject() returns TRUE for subject objects", {
  s <- subject_create("Health")
  expect_true(is.subject(s))
})

test_that("is.subject() returns FALSE for non-subject objects", {
  expect_false(is.subject(list(term = "NotClassed")))
  expect_false(is.subject("Just a string"))
})

test_that("subject() retrieves subject from attr or bibentry", {
  df <- dataset_df(data.frame(x = 1:3))
  subject(df) <- subject_create("Biodiversity")
  expect_equal(subject(df)$term, "Biodiversity")
})

test_that("subject() falls back to bibentry$subject", {
  df <- dataset_df(data.frame(x = 1:3))
  subject(df) <- "Forestry"
  # Remove subject attr to force fallback
  attr(df, "subject") <- NULL
  expect_equal(subject(df), "Forestry")
})

test_that("subject() provides the default setting if nothing else
          is set", {
  df <- dataset_df(data.frame(x = 1:3))
  expect_equal(subject(df)$term, "data sets")
})

test_that("subject<- handles NULL and character input", {
  df <- dataset_df(data.frame(x = 1:3))

  # NULL input
  subject(df) <- NULL
  expect_equal(subject(df)$term, ":tba")

  # character input
  subject(df) <- "Oceanography"
  expect_equal(subject(df)$term, "Oceanography")
})

test_that("subject<- errors for invalid input types", {
  df <- dataset_df(data.frame(x = 1:3))
  expect_error(
    subject(df) <- list(term = "Invalid"),
    regexp = "value must be a created with 'subject_create"
  )
})

test_that("subject<- replaces both attr and bibentry field", {
  df <- dataset_df(data.frame(x = 1:3))
  s <- subject_create("Volcanology")
  subject(df) <- s

  ds_bibentry <- get_bibentry(df)
  expect_equal(ds_bibentry$subject, "Volcanology")
  expect_equal(attr(df, "subject")$term, "Volcanology")
})

