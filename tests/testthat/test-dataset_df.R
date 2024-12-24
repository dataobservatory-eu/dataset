test_that("dataset_df() works", {
  expect_equal(is.dataset_df(dataset_df(mtcars)), TRUE)
  expect_false(is.dataset_df(mtcars))
  expect_equal(get_bibentry(dataset_df(mtcars))$author, person("Author", "Unknown"))
  expect_equal(dataset_df(mtcars, identifier = c(mt="http:/mtcars.com/dataset#"))$rowid,
               defined(paste0("mt:", 1:nrow(mtcars)), namespace=c(mt="http:/mtcars.com/dataset#")))
  my_dataset <- dataset_df(
    country_name = defined(
      c("AD", "LI"),
      definition = "http://data.europa.eu/bna/c_6c2bb82d",
      namespace = "https://www.geonames.org/countries/$1/"),
    gdp = defined(
      c(3897, 7365),
      label = "Gross Domestic Product",
      unit = "million dollars",
      definition = "http://data.europa.eu/83i/aa/GDP")
  )
  expect_equal(var_label(my_dataset$gdp), "Gross Domestic Product")
})


test_that("dataset_df() works", {
  test_dataset <- dataset_df(a=3, dataset_bibentry = datacite(Title="Hello", Creator = "Jane Doe"))
  expect_equal(get_bibentry(test_dataset)$author, person("Jane", "Doe"))
  expect_true(is.subject(subject(test_dataset)))
})

test_that("subsetting works", {
  expect_equal(ncol(iris_dataset[, 1]), 1)
  expect_equal(nrow(iris_dataset[1,2]), 1)
  expect_equal(iris$Sepal.Length[1], as.numeric(iris_dataset[1,2]) )
})

test_that("new_my_tibble() works", {
  myiris <- new_my_tibble(x=iris, identifier="example")
  expect_error(new_my_tibble(2))
  expect_equal(class(new_my_tibble(iris, identifier = "example")), c("dataset_df", "tbl_df", "tbl", "data.frame"))
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

test_that("print.dataset_df() works", {
  expect_output(print(iris_dataset), "E. Anderson.", ignore.case = FALSE)
})

test_that("as_dataset_df() works", {
  expect_equal(is.dataset_df(as_dataset_df(iris)), TRUE)
  expect_false(is.dataset_df(mtcars))
})


test_that("summary.dataset_df() works", {
  test_dataset <- dataset_df(a=3, dataset_bibentry = datacite(Title="Hello", Creator = "Jane Doe"))
  expect_output(summary(test_dataset), "Hello.", ignore.case = FALSE)
  expect_output(summary(test_dataset), "Doe J", ignore.case = FALSE)
})

test_that("names.dataset_df() works", {
  expect_output(print(names(iris_dataset)), "rowid", ignore.case = FALSE)
  expect_output(print(names(iris_dataset)), "Sepal.Length", ignore.case = FALSE)
  expect_length(names(iris_dataset), 6)
  expect_equal(names(iris_dataset)[1], "rowid")
})


