test_that("dataset_df sets default provenance on creation", {
  data("gdp", package = "dataset")

  test_dataset <- dataset_df(
    rowid = defined(paste0("eg:", 1:2), namespace = "http://example.com/dataset#"),
    geo = defined(
      gdp$geo[1:2],
      label = "Geopolitical entity",
      concept = "http://purl.org/linked-data/sdmx/2009/dimension#refArea",
      namespace = "https://dd.eionet.europa.eu/vocabulary/eurostat/geo/$1"
    ),
    dataset_bibentry = dublincore(
      title = "Test Dataset",
      creator = person("Daniel", "Antal",
        comment = c(ORCID = "0000-0001-7513-6760")
      ),
      publisher = "Test Publisher"
    )
  )

  prov <- provenance(test_dataset)

  # Check that some default triples are included
  expect_type(prov, "character")
  expect_true(any(grepl("prov#Entity", prov)))
  expect_true(any(grepl("prov#Agent", prov)))
  expect_true(any(grepl("generatedAtTime", prov)))
})


test_that("provenance(x) works", {
  expect_error(provenance(mtcars))
  tested <- dataset_df(x = mtcars)
  expect_true(class(provenance(tested)[1]) == "character")
  expect_output(print(provenance(tested)), "<http://example.com/dataset#>")
  provenance(x = tested) <- n_triple("https://doi.org/10.5281/zenodo.10396807", "http://www.w3.org/ns/prov#wasInformedBy", "http://example.com/source#1")
  expect_output(print(provenance(tested)), "<http://example.com/source#1>")
})
