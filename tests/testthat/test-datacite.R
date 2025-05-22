test_that("datacite() works", {
  iris_datacite <- datacite(
    Title = "Iris Dataset",
    Creator = person(family = "Anderson", given = "Edgar", role = "aut"),
    Publisher = "American Iris Society",
    PublicationYear = 1935,
    Geolocation = "US",
    Language = "en"
  )
  expect_true(is.datacite(iris_datacite))
  expect_equal(iris_datacite$language, "en")
  expect_equal(iris_datacite$geolocation, "US")
  expect_equal(iris_datacite$rights, ":tba")
  expect_equal(iris_datacite$subject, "data sets")
})

test_that("as_datacite() works", {
  expect_type(as_datacite(iris_dataset, "list"), "list")
  expect_true(is.dataset_df(as_datacite(iris_dataset, "dataset_df")))
  expect_warning(as_datacite(x = iris_dataset, type = "data.frame"))
  expect_equal(as_datacite(iris_dataset, "list")$FundingReference, ":unas")
  expect_equal(as_datacite(iris_dataset, "list")$Description, "The famous (Fisher's or Anderson's) iris data set.")
})

test_that("datacite() constructor works with structured person objects", {
  dc_obj <- datacite(
    Title = "Growth of Orange Trees",
    Creator = c(
      person(
        given = "N.R.",
        family = "Draper",
        role = "cre",
        comment = c(VIAF = "http://viaf.org/viaf/84585260")
      ),
      person(
        given = "H",
        family = "Smith",
        role = "cre"
      )
    ),
    Contributor = person(
      given = "Daniel",
      family = "Antal",
      role = "dtm"
    ),
    Publisher = "Wiley",
    Identifier = "https://doi.org/10.5281/zenodo.14917851",
    PublicationYear = 1998,
    Language = "en",
    Format = "application/r-rds",
    Rights = "CC-BY",
    Description = "Tree growth dataset over years."
  )

  expect_s3_class(dc_obj, "datacite")
  expect_s3_class(dc_obj, "bibentry")

  expect_equal(dc_obj$title, "Growth of Orange Trees")

  expect_equal(dc_obj$author, c(
    person(given = "N.R.", family = "Draper", role = "cre", comment = c(VIAF = "http://viaf.org/viaf/84585260")),
    person(given = "H", family = "Smith", role = "cre")
  ))

  expect_equal(dc_obj$publisher, "Wiley")
  expect_equal(dc_obj$identifier, "https://doi.org/10.5281/zenodo.14917851")
  expect_equal(dc_obj$year, "1998")
  expect_equal(dc_obj$language, "en")
  expect_equal(dc_obj$format, "application/r-rds")
  expect_equal(dc_obj$rights, "CC-BY")
  expect_equal(dc_obj$description, "Tree growth dataset over years.")

  # Contributor should be stored as an attribute (structured)
  contributor_attr <- attr(dc_obj, "contributor")
  expect_s3_class(contributor_attr, "person")
  expect_equal(contributor_attr, person(given = "Daniel", family = "Antal", role = "dtm"))
})

test_that("print.datacite outputs key fields", {
  dc <- datacite(
    Title = "Orange Tree Dataset",
    Creator = person(given = "Jane", family = "Doe"),
    Contributor = person(given = "John", family = "Smith", role = "ctb"),
    Publisher = "FruitLab",
    Identifier = "https://doi.org/10.1234/oranges",
    Date = 2023,
    Language = "en",
    Description = "A dataset measuring the growth of orange trees."
  )

  out <- capture.output(print(dc))
  expect_true(any(grepl("DataCite Metadata Record", out)))
  expect_true(any(grepl("Jane Doe", out)))
  expect_true(any(grepl("John Smith", out)))
  expect_true(any(grepl("FruitLab", out)))
})



test_that("datacite_to_triples works with minimal input", {
  input <- list(title = "Test Dataset")
  out <- datacite_to_triples(input)
  expect_type(out, "character")
  expect_length(out, 1)
  expect_true(grepl("datacite.org/schema/kernel-4/title", out[1]))
})

test_that("datacite_to_triples works with full metadata", {
  input <- list(
    title = "Test Dataset",
    creator = "Jane Doe",
    contributor = "John Smith",
    identifier = "https://doi.org/10.1234/test",
    publisher = "Test Publisher",
    publicationyear = "2024",
    language = "en",
    rights = "CC-BY",
    description = "A full test.",
    subject = "Science",
    format = "application/csv",
    version = "1.0"
  )
  out <- datacite_to_triples(input)
  expect_length(out, 12)
  expect_true(all(grepl("datacite.org/schema/kernel-4/", out)))
})

test_that("datacite_to_triples skips missing optional fields", {
  input <- list(title = "Partial Dataset", creator = "Jane Doe")
  out <- datacite_to_triples(input)
  expect_length(out, 2)
  expect_true(any(grepl("creator", out)))
  expect_false(any(grepl("contributor", out)))
})

test_that("datacite_to_triples errors if title is missing", {
  input <- list(creator = "Jane Doe")
  expect_error(datacite_to_triples(input), "title is required")
})

test_that("datacite_to_triples uses custom dataset_id", {
  input <- list(title = "With ID")
  out <- datacite_to_triples(input, dataset_id = "http://example.org/custom")
  expect_true(all(grepl("^<http://example.org/custom>", out)))
})

