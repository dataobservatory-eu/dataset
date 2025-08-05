test_that("as_dublincore() handles multiple creators and metadata correctly", {
  orange_bibentry <- dublincore(
    title = "Growth of Orange Trees",
    creator = c(
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
    contributor = person(
      given = "Antal",
      family = "Daniel",
      role = "dtm"
    ),
    publisher = "Wiley",
    datasource = "https://isbnsearch.org/isbn/9780471170822",
    dataset_date = 1998,
    identifier = "https://doi.org/10.5281/zenodo.14917851",
    language = "en",
    description = "The Orange data frame has 35 rows and 3 columns of records of the growth of orange trees."
  )

  test <- dataset_df(a = 1:3, dataset_bibentry = orange_bibentry)

  dc <- as_dublincore(test, type = "ntriples")

  expect_type(dc, "character")
  expect_true(all(grepl("^<https://doi.org/10.5281/zenodo.14917851> <http://purl.org/dc/terms/", dc)))

  # Check presence of key predicates
  expect_true(any(grepl("dc/terms/title", dc)))
  expect_true(any(grepl("dc/terms/creator", dc)))
  expect_true(any(grepl("dc/terms/contributor", dc)))
  expect_true(any(grepl("dc/terms/publisher", dc)))
  expect_true(any(grepl("dc/terms/identifier", dc)))
  expect_true(any(grepl("dc/terms/source", dc)))
  expect_true(any(grepl("dc/terms/description", dc)))
  expect_true(any(grepl("dc/terms/date", dc)))
  expect_true(any(grepl("dc/terms/language", dc)))
  expect_true(any(grepl("dc/terms/type", dc)))

  # Specific value checks
  expect_true(any(grepl("Draper", dc)))
  expect_true(any(grepl("Smith", dc)))
  expect_true(any(grepl("Daniel", dc)))
  expect_true(any(grepl("Wiley", dc)))
  expect_true(any(grepl("zenodo.14917851", dc)))
  expect_true(any(grepl("Growth of Orange Trees", dc)))
  expect_true(any(grepl("Orange data frame", dc)))
  expect_true(any(grepl("9780471170822", dc)))
})
