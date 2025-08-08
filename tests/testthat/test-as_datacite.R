test_that("returns datacite bibentry by default", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- datacite(
    Title = "Test Dataset",
    Creator = person("Jane", "Doe")
  )
  result <- as_datacite(df)
  expect_s3_class(result, "datacite")
  expect_s3_class(result, "bibentry")
})

test_that("returns list if type = 'list'", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- datacite(
    Title = "List Output",
    Creator = person("Jane", "Doe")
  )
  result <- as_datacite(df, type = "list")
  expect_type(result, "list")
  expect_named(result, c(
    "Title", "Creator", "Identifier", "Publisher", "PublicationYear",
    "Subject", "Type", "Contributor", "Date", "Language",
    "AlternateIdentifier", "RelatedIdentifier", "Format", "Version",
    "Rights", "Description", "Geolocation", "FundingReference"
  ))
})

test_that("returns dataset_df if type = 'dataset_df'", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- datacite(
    Title = "Structured Output",
    Creator = person("Jane", "Doe"),
    Identifier = "doi:10.1234/example",
    Publisher = "DataPub",
    Subject = subject_create(
      term = "Oranges",
      schemeURI = "http://id.loc.gov/authorities/subjects",
      valueURI = "http://id.loc.gov/authorities/subjects/sh85095257",
      subjectScheme = "LCCH",
      prefix = "lcch:"
    ),
    Contributor = person("Contributor", "Name"),
    Date = "2022",
    Language = "en",
    AlternateIdentifier = "alt-id",
    RelatedIdentifier = "rel-id",
    Format = "text/csv",
    Version = "v1.0",
    Rights = "CC-BY",
    Description = "A dataset about something",
    Geolocation = "Global",
    FundingReference = "Funder X"
  )
  result <- as_datacite(df, type = "dataset_df")
  expect_s3_class(result, "dataset_df")
  expect_true(all(c("Title", "Creator", "Identifier", "Publisher") %in% names(result)))
})

test_that("returns ntriples string if type = 'ntriples'", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- datacite(
    Title = "Triple Output",
    Creator = person("Jane", "Doe"),
    Contributor = person("Contributor", "Name")
  )
  result <- as_datacite(df, type = "ntriples")
  expect_type(result, "character")
  expect_true(grepl("http", result[1]))
})

test_that("invalid type falls back to bibentry with warning", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- datacite(
    Title = "Fallback",
    Creator = person("Jane", "Doe")
  )
  expect_warning(
    result <- as_datacite(df, type = "nonsense"),
    "type cannot be"
  )
  expect_s3_class(result, "datacite")
})

test_that("errors if author argument is not a person", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- datacite(
    Title = "Bad Author",
    Creator = person("Jane", "Doe")
  )
  expect_error(
    as_datacite(df, author = "not a person"),
    "author must be created with utils\\:\\:person"
  )
})
