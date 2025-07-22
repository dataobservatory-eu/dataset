test_that("dataset_to_triples expands subject URIs correctly", {
  data("gdp", package = "dataset")

  small_country_dataset <- dataset_df(
    geo = defined(
      gdp$geo,
      label = "Country name",
      concept = "http://dd.eionet.europa.eu/vocabulary/eurostat/geo/",
      namespace = "https://www.geonames.org/countries/$1/"
    ),
    year = defined(
      gdp$year,
      label = "Reference Period (Year)",
      concept = "http://purl.org/linked-data/sdmx/2009/dimension#refPeriod"
    ),
    gdp = defined(
      gdp$gdp,
      label = "Gross Domestic Product",
      unit = "CP_MEUR",
      concept = "http://data.europa.eu/83i/aa/GDP"
    ),
    unit = gdp$unit,
    freq = defined(
      gdp$freq,
      label = "Frequency",
      concept = "http://purl.org/linked-data/sdmx/2009/code"
    ),
    dataset_bibentry = dublincore(
      title = "Small Country Dataset",
      creator = person("Jane", "Doe"),
      publisher = "Example Inc.",
      datasource = "https://doi.org/10.2908/NAIDA_10_GDP",
      rights = "CC-BY",
      coverage = "Andorra, Lichtenstein and San Marino"
    )
  )

  triples <- dataset_to_triples(small_country_dataset)

  # Extract actual s values
  s_values <- unique(triples$s)
  # Expect all s values to begin with the namespace prefix
  expect_true(all(grepl("^http://example\\.com/dataset#", s_values)))

 # Check that s is URI-expanded
  expect_false(any(s_values %in% gdp$geo))  # none should be raw codes like "AD"
})


test_that("triples_column_generate generates correct RDF triples", {
  s <- c("http://example.com/obs/1", "http://example.com/obs/2")

  # Case 1: defined with namespace and concept
  x <- defined(
    c("AD", "FR"),
    label = "Country",
    concept = "http://example.com/prop/geo",
    namespace = "https://eurostat.europa.eu/geo/$1"
  )
  triples <- triples_column_generate(s, x, "geo")

  expect_equal(triples$p[1], "http://example.com/prop/geo")
  expect_equal(triples$o[2], "https://eurostat.europa.eu/geo/FR")

  # Case 2: numeric without namespace
  x <- c(1.23, 4.56)
  triples <- triples_column_generate(s, x, "value")
  expect_true(all(grepl('^".+"\\^\\^<xs:decimal>$', triples$o)))
})

test_that("dataset_to_triples generates valid N-Triples", {
  data("gdp", package = "dataset")

  small_country_dataset <- dataset_df(
    geo = defined(
      gdp$geo,
      label = "Country name",
      concept = "http://example.com/prop/geo",
      namespace = "https://www.geonames.org/countries/$1/"
    ),
    year = defined(
      gdp$year,
      label = "Reference Period (Year)",
      concept = "http://purl.org/linked-data/sdmx/2009/dimension#refPeriod"
    ),
    gdp = defined(
      gdp$gdp,
      label = "Gross Domestic Product",
      unit = "CP_MEUR",
      concept = "http://data.europa.eu/83i/aa/GDP"
    ),
    unit = defined(
      gdp$unit,
      label = "Unit of measure",
      concept = "http://purl.org/linked-data/sdmx/2009/attribute#unitMeasure",
      namespace = "https://dd.eionet.europa.eu/vocabulary/eurostat/unit/$1"
    ),
    freq = defined(
      gdp$freq,
      label = "Frequency",
      concept = "http://purl.org/linked-data/sdmx/2009/code#freq",
      namespace = "http://purl.org/linked-data/sdmx/2009/code#freq-"
    ),
    dataset_bibentry = dublincore(
      title = "Small Country Dataset",
      creator = person("Jane", "Doe"),
      publisher = "Example Inc.",
      datasource = "https://doi.org/10.2908/NAIDA_10_GDP",
      rights = "CC-BY",
      coverage = "Andorra, Lichtenstein and San Marino"
    )
  )

  # Get expanded triple table and N-Triples
  triples_df <- dataset_to_triples(small_country_dataset)
  ntriples <- dataset_to_triples(small_country_dataset, format = "nt")

  # Validate structure
  expect_true(all(c("s", "p", "o") %in% names(triples_df)))
  expect_true(all(grepl("^<.*> <.*> .+ \\.$", ntriples)))  # basic N-Triples format

  # Spot-check GDP literal
  gdp_triples <- subset(triples_df, grepl("GDP", p))
  expect_true(all(grepl('\\^\\^<xs:decimal>$', gdp_triples$o)))

  # Spot-check geo URIs
  geo_triples <- subset(triples_df, grepl("prop/geo", p))
  expect_true(all(grepl("^https://www\\.geonames\\.org/countries/", geo_triples$o)))

  # Spot-check unit URIs
  unit_triples <- subset(triples_df, grepl("unitMeasure", p))
  expect_true(all(grepl("^https://dd\\.eionet\\.europa\\.eu/vocabulary/eurostat/unit/", unit_triples$o)))

  # Check frequency URIs
  freq_triples <- subset(triples_df, grepl("code#freq", p))
  expect_true(all(grepl("^http://purl\\.org/linked-data/sdmx/2009/code#freq-", freq_triples$o)))

  # Ensure N-Triples length matches data frame
  expect_equal(length(ntriples), nrow(triples_df))
})



