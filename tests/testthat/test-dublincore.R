test_that("creates valid dublincore object", {
  dc <- dublincore(
    title = "Sample Dataset",
    creator = person("Jane", "Doe")
  )
  expect_s3_class(dc, c("dublincore", "bibrecord", "bibentry"))
  expect_equal(dc$title, "Sample Dataset")
  expect_true(inherits(dc$author[[1]], "person"))
})

test_that("dublincore() errors when title has length > 1", {
  expect_error(
    dublincore(
      title = c("Too", "Many", "Titles"),
      creator = person("Jane", "Doe")
    ),
    "title must be a single character string"
  )
})


test_that("missing creator fails", {
  expect_error(
    dublincore(title = "Missing Creator"),
    "creator.*required"
  )
})

test_that("default values are set", {
  dc <- dublincore(
    title = "Defaults Example",
    creator = person("Jane", "Doe")
  )
  expect_equal(dc$rights, ":tba")
  expect_equal(dc$format, "application/r-rds")
  expect_equal(dc$relation, ":unas")
})

test_that("custom contributor is set", {
  contrib <- person(given = "Antal", family = "Daniel", role = "dtm")
  dc <- dublincore(
    title = "Contrib Example",
    creator = person("Jane", "Doe"),
    contributor = contrib
  )
  expect_equal(attr(dc, "contributor")[[1]]$given, "Antal")
  expect_equal(attr(dc, "contributor")[[1]]$role, "dtm")
})

test_that("dataset_date sets year correctly", {
  dc <- dublincore(
    title = "Date Example",
    creator = person("Jane", "Doe"),
    dataset_date = as.Date("2022-06-01")
  )
  expect_equal(dc$year, "2022")
})

test_that("identifier, language, and format are preserved", {
  dc <- dublincore(
    title = "Complete Example",
    creator = person("Jane", "Doe"),
    identifier = "https://doi.org/10.1234/test",
    language = "en",
    dataset_format = "application/json"
  )
  expect_equal(dc$identifier, "https://doi.org/10.1234/test")
  expect_equal(dc$language, "en")
  expect_equal(dc$format, "application/json")
})

test_that("print.dublincore prints key fields", {
  dc <- dublincore(
    title = "Printed Example",
    creator = person("Jane", "Doe"),
    publisher = "Example Org",
    dataset_date = 2020,
    language = "en",
    description = "Some description."
  )
  expect_output(print(dc), "Dublin Core Metadata Record")
  expect_output(print(dc), "Title:.*Printed Example")
  expect_output(print(dc), "Creator.*Doe")
  expect_output(print(dc), "Publisher:.*Example Org")
  expect_output(print(dc), "Year:.*2020")
  expect_output(print(dc), "Language:.*en")
  expect_output(print(dc), "Description:.*Some description")
})

test_that("is.dublincore returns TRUE for dublincore object", {
  dc <- dublincore(
    title = "Dublin Check",
    creator = person("Jane", "Doe")
  )
  expect_true(is.dublincore(dc))
})

test_that("is.dublincore returns FALSE for non-dublincore object", {
  df <- data.frame(x = 1:3)
  expect_false(is.dublincore(df))
})
