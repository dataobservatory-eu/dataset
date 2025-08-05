test_that("as_dublincore() gives warning", {
  expect_warning(as_dublincore(iris_dataset, type = "character"))
})

test_that("as_dublincore returns valid N-Triples from dataset_df", {
  x <- dataset_df(
    rowid = defined(1:2),
    val = defined(c("A", "B")),
    dataset_bibentry = dublincore(
      title = "Demo Dataset",
      creator = person("Jane", "Doe"),
      publisher = "Example Publisher",
      identifier = "http://example.org/demo"
    )
  )

  dc <- as_dublincore(x, type = "ntriples")

  expect_type(dc, "character")
  expect_true(all(grepl("^<http://example.org/demo> <http://purl.org/dc/terms/", dc)))

  # Check that key triples are present
  expect_true(any(grepl("dc/terms/title", dc)))
  expect_true(any(grepl("dc/terms/creator", dc)))
  expect_true(any(grepl("dc/terms/publisher", dc)))
  expect_true(any(grepl("dc/terms/identifier", dc)))
  expect_true(any(grepl("dc/terms/type", dc)))
  expect_true(any(grepl("Demo Dataset", dc)))

  expect_true(any(grepl('"Demo Dataset"\\^\\^<http://www.w3.org/2001/XMLSchema#string>', dc)))
})
