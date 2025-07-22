test_that("describe() writes provenance and metadata to file", {
  data("gdp", package = "dataset")

  test_ds <- dataset_df(
    rowid = defined(c("eg:1", "eg:2"), namespace = "http://example.com/dataset#"),
    geo = defined(
      gdp$geo[1:2],
      label = "Country",
      concept = "http://example.com/prop/geo",
      namespace = "https://eionet.europa.eu/geo/$1"
    ),
    dataset_bibentry = dublincore(
      title = "Example Dataset",
      creator = person("John", "Doe")
    )
  )

  tmpfile <- tempfile(fileext = ".nt")
  describe(x=test_ds, con=tmpfile)

  lines <- readLines(tmpfile)
  expect_true(any(grepl("prov#", lines)))
  expect_true(any(grepl("ttp://purl.org/dc/terms/title", lines)))
  expect_true(length(lines) > 3)
})

x = orange_df
con = tempfile()
