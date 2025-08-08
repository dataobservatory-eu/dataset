test_that("datacite() creates a datacite object with minimal input", {
  dc <- datacite(
    Title = "A Simple Dataset",
    Creator = person("John", "Doe", role = "cre"),
    Publisher = "Test Publisher",
    Date = "2024-01-01",
    Language = "en"
  )

  expect_s3_class(dc, "datacite")
  expect_s3_class(dc, "bibentry")
  expect_equal(dc$title, "A Simple Dataset")
  expect_equal(dc$publisher, "Test Publisher")
  expect_equal(dc$year, "2024")
  expect_equal(dc$language, "en")
  expect_equal(dc$format, ":tba")
})

test_that("datacite() calculates PublicationYear from Date if not supplied", {
  dc <- datacite(
    Title = "Auto-Year Dataset",
    Creator = person("Jane", "Smith", role = "cre"),
    Publisher = "Test Publisher",
    Date = "2017-06-01"
  )
  expect_equal(dc$year, "2017")
})

test_that("datacite() accepts subject from subject_create()", {
  subject_obj <- subject_create("Climate Change", subjectScheme = "LCSH")
  dc <- datacite(
    Title = "Climate Data",
    Creator = person("Eve", "Rivera", role = "cre"),
    Publisher = "Climate Org",
    Subject = subject_obj
  )
  expect_equal(dc$subject, "Climate Change")
})

test_that("datacite() handles optional identifiers and contributors", {
  contributor <- person("Sam", "Lee", role = "ctb")
  dc <- datacite(
    Title = "Dataset with DOI",
    Creator = person("John", "Doe", role = "cre"),
    Identifier = "https://doi.org/10.1234/testdoi",
    Publisher = "OrgX",
    Contributor = contributor
  )
  expect_equal(dc$identifier, "https://doi.org/10.1234/testdoi")
  expect_equal(attr(dc, "contributor")[[1]]$family, "Lee")
})

test_that("datacite() fills defaults when values are NULL", {
  dc <- datacite(
    Title = "Null Defaults",
    Creator = person("Ana", "Ng", role = "cre"),
    Publisher = "Default Org"
  )
  expect_equal(dc$description, ":tba")
  expect_equal(dc$rights, ":tba")
  expect_equal(dc$geolocation, ":unas")
  expect_equal(dc$format, ":tba")
  expect_equal(dc$alternateidentifier, ":unas")
})

test_that("is.datacite() and print.datacite() behave correctly", {
  dc <- datacite(
    Title = "Printable",
    Creator = person("Joe", "Bloggs", role = "cre"),
    Publisher = "Printed"
  )
  expect_true(is.datacite(dc))
  expect_output(print(dc), "DataCite Metadata Record")
})

test_that("datacite_to_triples() fails when title is missing", {
  dc <- list() # missing required field
  expect_error(
    datacite_to_triples(dc),
    regexp = "title is required"
  )
})
