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
  expect_equal(attr(dc, "subject"), subject_obj)
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

test_that("datacite stores structured related as attribute,
          flat string in slot", {
  rel <- related_create("10.1234/abc",
    relationType =
      "IsPartOf", relatedIdentifierType = "DOI"
  )
  dc <- datacite(
    Title = "X",
    Creator = person("A", "B", role = "cre"),
    Publisher = "Org",
    Subject = subject_create("Test"),
    RelatedIdentifier = rel
  )
  expect_equal(dc$relatedidentifier, "10.1234/abc") # flat string
  expect_s3_class(attr(dc, "relatedIdentifier"), "related") # structured
  expect_equal(attr(dc, "relatedIdentifier")$relationType, "IsPartOf")
})


test_that("is.datacite() behave as expected", {
  dc <- datacite(
    Title = "Null Defaults",
    Creator = person("Ana", "Ng", role = "cre"),
    Publisher = "Default Org"
  )
  expect_true(is.datacite(dc))
})

test_that("print.datacite() behaves as expected", {
  subject_obj <- subject_create("Climate Change", subjectScheme = "LCSH")
  dc <- datacite(
    Title = "Climate Data",
    Creator = person("Eve", "Rivera", role = "cre"),
    Publisher = "Climate Org",
    Subject = subject_obj
  )
  expect_output(print(dc), "DataCite Metadata Record")

  out <- capture_output(print(dc))

  # Header
  expect_match(out, "DataCite Metadata Record")
  expect_match(out, "--------------------------")

  # Fields
  expect_match(out, "Title:\\s+Climate Data")
  expect_match(out, "Creator\\(s\\):\\s+Eve Rivera \\[cre\\]")
  expect_match(out, "Subject\\(s\\):\\s+Climate Change")
  expect_match(out, "Publisher:\\s+Climate Org")
  expect_match(out, "Year:\\s+:tba")
  expect_match(out, "Description:\\s+:tba")

  # Alignment: all labels should have the colon at same position
  lines <- strsplit(out, "\n")[[1]]
  meta_lines <- grep(":", lines, value = TRUE)

  # find where values start (first non-space after colon)
  value_start <- vapply(meta_lines, function(l) {
    m <- regexpr(":", l)
    rest <- substr(l, m + 1, nchar(l))
    m + regexpr("[^ ]", rest)
  }, integer(1))

  expect_true(length(unique(value_start)) == 1)
})

test_that("datacite_to_triples() fails when title is missing", {
  dc <- list() # missing required field
  expect_error(
    datacite_to_triples(dc),
    regexp = "title is required"
  )
})
