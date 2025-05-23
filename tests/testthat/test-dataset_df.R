test_that("dataset_df() works", {
  expect_s3_class(dataset_df(mtcars), "dataset_df")
  expect_true(is.dataset_df(dataset_df(mtcars)))
  expect_false(is.dataset_df(mtcars))
  expect_equal(get_bibentry(dataset_df(mtcars))$author, person("Author", "Unknown"))
  expect_equal(
    dataset_df(mtcars, identifier = c(mt = "http:/mtcars.com/dataset#"))$rowid,
    defined(paste0("mt:", seq_along(mtcars$mpg)),
      namespace = c(mt = "http:/mtcars.com/dataset#")
    )
  )
  my_dataset <- dataset_df(
    country_name = defined(
      c("AD", "LI"),
      concept = "http://data.europa.eu/bna/c_6c2bb82d",
      namespace = "https://www.geonames.org/countries/$1/"
    ),
    gdp = defined(
      c(3897, 7365),
      label = "Gross Domestic Product",
      unit = "million dollars",
      concept = "http://data.europa.eu/83i/aa/GDP"
    )
  )
  expect_equal(var_label(my_dataset$gdp), "Gross Domestic Product")
})

test_that("dataset_df() works", {
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
    ), # Add data manager
    identifier = "https://doi.org/10.5281/zenodo.10396807", #
    publisher = "Wiley",
    datasource = "https://isbnsearch.org/isbn/9780471170822",
    dataset_date = 1998,
    language = "en",
    description = "The Orange data frame has 35 rows and 3 columns of records of the growth of orange trees."
  )
  orange_df <- dataset_df(
    rowid = defined(paste0("orange:", row.names(Orange)),
      label = "ID in the Orange dataset",
      namespace = c("orange" = "datasets::Orange")
    ),
    tree = defined(Orange$Tree,
      label = "The number of the tree"
    ),
    age = defined(Orange$age,
      label = "The age of the tree",
      unit = "days since 1968/12/31"
    ),
    circumference = defined(Orange$circumference,
      label = "circumference at breast height",
      unit = "milimeter",
      concept = "https://www.wikidata.org/wiki/Property:P2043"
    ),
    dataset_bibentry = orange_bibentry
  )
  expect_equal(dataset_title(orange_df), "Growth of Orange Trees")
  expect_equal(creator(orange_df), c(c(
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
  )))
})

test_that("dataset_df() works", {
  test_dataset <- dataset_df(
    a = 3,
    dataset_bibentry = datacite(
      Title = "Hello",
      Creator = person("Jane", "Doe"),
      PublicationYear = 2025
    )
  )
  expect_equal(get_bibentry(test_dataset)$author, person("Jane", "Doe"))
  expect_true(is.subject(subject(test_dataset)))
})

test_that("subsetting works", {
  expect_equal(ncol(iris_dataset[, 1]), 1)
  expect_equal(nrow(iris_dataset[1, 2]), 1)
  expect_equal(iris$Sepal.Length[1], as_numeric(iris_dataset[[1, 2]]))
})

test_that("new_dataset() works", {
  myiris <- new_dataset(x = iris, identifier = "example")
  expect_error(new_dataset(2))
  expect_equal(class(new_dataset(iris, identifier = "example")), c("dataset_df", "tbl_df", "tbl", "data.frame"))
  expect_output(print(provenance(myiris)), "<http://example.com/dataset#>")
})

test_that("is.dataset_df() works", {
  expect_true(is.dataset_df(iris_dataset))
  expect_false(is.dataset_df(mtcars))
})

test_that("rbind works", {
  iris_dataset1 <- iris_dataset
  iris_dataset2 <- iris_dataset
  expect_equal(nrow(rbind(iris_dataset1, iris_dataset2)), 300)
})

test_that("print.dataset_df prints citation, column names, and variable labels", {
  df <- dataset_df(
    code = defined(c("A", "B"), label = "Code Label"),
    value = defined(c(10, 20), label = "Value Label", unit = "units"),
    dataset_bibentry = dublincore(title = "Untitled Dataset", creator = person("Jane", "Doe", role = "ctb"))
  )

  output <- capture.output(print(df))

  # Check citation
  expect_true(any(grepl("Untitled Dataset", output)))

  # Check column headers
  expect_true(any(grepl("\\bcode\\b", output)))
  expect_true(any(grepl("\\bvalue\\b", output)))

  # Check that label line contains truncated or full label prefixes
  label_row <- output[which.max(grepl("rowid", output)) + 1]
  expect_true(grepl("Code L", label_row)) # relaxed match
  expect_true(grepl("Value L", label_row)) # relaxed match
})



test_that("as_dataset_df() works", {
  expect_s3_class(as_dataset_df(iris), "dataset_df")
  expect_false(is.dataset_df(mtcars))
})


test_that("summary.dataset_df() works", {
  test_dataset <- dataset_df(
    a = 3,
    dataset_bibentry = datacite(
      Title = "Hello",
      Creator = person("Jane", "Doe"),
      PublicationYear = 2024
    )
  )
  expect_output(summary(test_dataset), "Hello", ignore.case = FALSE)
  expect_output(summary(test_dataset), "Jane Doe", ignore.case = FALSE)
})


test_that("names.dataset_df() works", {
  expect_output(print(names(iris_dataset)), "rowid", ignore.case = FALSE)
  expect_output(print(names(iris_dataset)), "Sepal.Length", ignore.case = FALSE)
  expect_length(names(iris_dataset), 6)
  expect_equal(names(iris_dataset)[1], "rowid")
})
