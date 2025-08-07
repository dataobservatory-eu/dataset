test_that("identifier() retrieves the identifier from dataset_df", {
  df <- dataset_df(data.frame(x = 1:3))
  identifier(df) <- "http://example.org/dataset/123"
  expect_equal(identifier(df), "http://example.org/dataset/123")
})

test_that("identifier() retrieves the identifier from bibentry", {
  be <- dublincore(title = "Example", creator = person("Jane", "Doe"))
  identifier(be) <- "http://example.org/resource"
  expect_equal(identifier(be), "http://example.org/resource")
})

test_that("identifier<- sets DOI and extracts correctly", {
  df <- dataset_df(data.frame(x = 1:3))
  identifier(df) <- "https://doi.org/10.1234/example.doi"
  expect_equal(identifier(df), "https://doi.org/10.1234/example.doi")
  expect_equal(attr(df, "doi"), "10.1234/example.doi")
})

test_that("identifier<- accepts unnamed character string", {
  df <- dataset_df(data.frame(x = 1:3))
  identifier(df) <- "https://example.org/id/abc123"
  expect_equal(identifier(df), "https://example.org/id/abc123")
})

test_that("identifier<- accepts numeric and coerces to character", {
  df <- dataset_df(data.frame(x = 1:3))
  identifier(df) <- 12345
  expect_equal(identifier(df), "12345")
})

test_that("identifier<- sets default ':unas' on NULL", {
  df <- dataset_df(data.frame(x = 1:3))
  identifier(df) <- NULL
  expect_equal(identifier(df), ":unas")
})

test_that("identifier<- doesn't overwrite unless allowed", {
  df <- dataset_df(data.frame(x = 1:3))
  identifier(df) <- "https://example.org/id/1"
  expect_warning(
    identifier(df, overwrite = FALSE) <- "https://example.org/id/2",
    regexp = "The dataset has already an identifier"
  )
  expect_equal(identifier(df), "https://example.org/id/1")
})

test_that("identifier<- overwrites when allowed", {
  df <- dataset_df(data.frame(x = 1:3))
  identifier(df) <- "https://example.org/id/1"
  identifier(df, overwrite = TRUE) <- "https://example.org/id/2"
  expect_equal(identifier(df), "https://example.org/id/2")
})

test_that("identifier<- errors on invalid object types", {
  expect_error(
    identifier(data.frame(x = 1)),
    regexp = "x must be a dataset_df or a bibentry object"
  )
  df <- data.frame(x = 1)
  expect_error(
    do.call("identifier<-", list(df, value = "id")),
    regexp = "x must be a dataset_df or a bibentry object"
  )
})

test_that("identifier<- errors on invalid value types", {
  df <- dataset_df(data.frame(x = 1:3))
  expect_error(
    identifier(df) <- list("invalid"),
    regexp = "value must be a named or not named character string"
  )
})
