test_that("returns dublincore bibentry by default", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- dublincore(
    title = "Test Dataset",
    creator = person("Jane", "Doe")
  )
  result <- as_dublincore(df)
  expect_s3_class(result, "dublincore")
  expect_s3_class(result, "bibentry")
})

test_that("returns list if type = 'list'", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- dublincore(
    title = "List Output",
    creator = person("Jane", "Doe")
  )
  result <- as_dublincore(df, type = "list")
  expect_type(result, "list")
  expect_named(result, c(
    "title", "creator", "identifier", "publisher", "subject", "type",
    "contributor", "date", "language", "relation", "dataset_format", "rights",
    "datasource", "description", "coverage"
  ))
})

test_that("returns dataset_df if type = 'dataset_df'", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- dublincore(
    title = "Structured Output",
    creator = person("Jane", "Doe"),
    identifier = "doi:10.1234/example",
    publisher = "DataPub",
    subject = "Science",
    contributor = person("Contributor", "Name"),
    dataset_date = "2022",
    language = "en",
    relation = "https://related.example.org",
    dataset_format = "text/csv",
    rights = "CC-BY",
    datasource = "https://example.org/data",
    description = "A dataset about something",
    coverage = "Global"
  )
  result <- as_dublincore(df, type = "dataset_df")
  expect_s3_class(result, "dataset_df")
  expect_true(all(c("title", "creator", "identifier", "publisher") %in% names(result)))
})

test_that("returns ntriples string if type = 'ntriples'", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- dublincore(
    title = "Triple Output",
    creator = person("Jane", "Doe"),
    contributor = person("Contributor", "Name")
  )
  result <- as_dublincore(df, type = "ntriples")
  expect_type(result, "character")
  expect_true(grepl("http", result[1]))
})

test_that("invalid type falls back to bibentry with warning", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- dublincore(
    title = "Fallback",
    creator = person("Jane", "Doe")
  )
  expect_warning(
    result <- as_dublincore(df, type = "nonsense"),
    "type cannot be"
  )
  expect_s3_class(result, "dublincore")
})

test_that("errors if author argument is not a person", {
  df <- dataset_df(x = 1:3)
  attr(df, "dataset_bibentry") <- dublincore(
    title = "Bad Author",
    creator = person("Jane", "Doe")
  )
  expect_error(
    as_dublincore(df, author = "not a person"),
    "author must be created with utils\\:\\:person"
  )
})
