defined(c(0, 1, 0, 1, 1, 0),
  label = "sex",
  labels = c("F" = 0, "M" = 1, "_N" = 99),
  definition = "https://registry.sdmx.org/sdmx/v2/structure/codelist/SDMX/CL_SEX/2.1#"
)


test_that("labelled_defined() works", {
  sepal_length <- defined(iris$Sepal.Length,
    labels = NULL,
    label = "Sepal length",
    unit = "centimeters",
    definition = "https://www.wikidata.org/wiki/Property:P2043"
  )
  myspecies <- defined(
    x = iris$Species,
    label = "Taxon name within the Iris genus",
    definition = "https://npgsweb.ars-grin.gov/gringlobal/taxon/taxonomygenus?id=6074",
    namespace = "Iris"
  )
  expect_equal(is.defined(sepal_length), TRUE)
  expect_equal(var_label(sepal_length), "Sepal length")
  expect_equal(as_character(defined(as.factor(c("Man", "Woman", "Woman", "Man")))), c("Man", "Woman", "Woman", "Man"))
  expect_equal(var_unit(sepal_length), "centimeters")
  expect_equal(var_definition(sepal_length), "https://www.wikidata.org/wiki/Property:P2043")
  expect_equal(var_namespace(myspecies), "Iris")
  expect_true(all(as.character(sepal_length) == as.character(iris$Sepal.Length)))
})

test_that("labelled_defined() throws error", {
  expect_error(var_unit(sepal_length) <- c("cm", "mm"))
  expect_error(var_unit(sepal_length) <- 1)
  expect_error(defined(
    x = iris$Species,
    label = "Taxon name within the Iris genus",
    definition = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = iris$Species,
    label = "Taxon name within the Iris genus",
    unit = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = iris$Species,
    label = "Taxon name within the Iris genus",
    namespace = 1
  ))
})

test_that("new_datetime_defined() throws errors", {
  expect_error(defined(
    x = Sys.Date(),
    label = c("Today's date", "Extra label"),
    definition = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = Sys.Date(),
    label = 1,
    definition = "Definition",
    namespace = "Iris"
  ))
  expect_error(defined(
    x = Sys.Date(),
    label = "Today's date",
    definition = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = Sys.Date(),
    label = "Today's date", ,
    unit = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = Sys.Date(),
    label = "Today's date",
    namespace = 1
  ))
})

test_that("new_labelled_defined() throws errors", {
  expect_error(defined(
    x = c(1:3),
    label = c("Numbers", "Numbers"),
    definition = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = c(1:3),
    label = "Numbers",
    definition = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = c(1:3),
    label = "Numbers",
    definition = 1,
    unit = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = c(1:3),
    label = "Numbers",
    namespace = 1
  ))
})

test_that("iris_dataset() prints", {
  expect_output(str(iris_dataset), "https://doi.org/10.5281/zenodo.10396807", ignore.case = FALSE)
  expect_output(print(iris_dataset), "Iris Dataset.", ignore.case = FALSE)
})

a <- defined(iris$Sepal.Length[1:3],
  labels = NULL,
  label = "Sepal length",
  unit = "centimeters",
  definition = "https://www.wikidata.org/wiki/Property:P2043"
)


b <- defined(iris$Sepal.Length[4:6],
  labels = NULL,
  label = "Sepal length",
  unit = "centimeters",
  definition = "https://www.wikidata.org/wiki/Property:P2043"
)

bmm <- defined(iris$Sepal.Length[7:9] * 10,
  labels = NULL,
  label = "Sepal length",
  unit = "milimeters",
  definition = "https://www.wikidata.org/wiki/Property:P2043"
)


test_that("c() works", {
  expect_equal(is.defined(c(a, b)), TRUE)
  expect_equal(length(c(a, b)), 6)
  expect_error(c(a, bmm))
})


test_that("summary.haven_labelled_defined() works ", {
  sepal_length <- iris_dataset$Sepal.Length
  expect_output(print(sepal_length), "Length of the sepal in cm")
  expect_output(summary(sepal_length), "Length of the sepal in cm \\(centimeter\\)")
  expect_equal(names(summary(sepal_length))[1], "Min.")
})

test_that("as_numeric.haven_labelled_defined() works ", {
  sepal_length <- defined(iris$Sepal.Length,
    labels = NULL,
    label = "Sepal length",
    unit = "centimeters",
    definition = "https://www.wikidata.org/wiki/Property:P2043"
  )
  expect_equal(as_numeric(sepal_length), iris$Sepal.Length)
  expect_equal(as_character(x = sepal_length), as.character(iris$Sepal.Length))
})

test_that("c.haven_labelled_defined() works ", {
  a <- defined(1:3, label = "testlabel", unit = "meter", definition = "testdef", namespace = "http://example.com")
  b <- defined(4:6, label = "testlabel", unit = "meter", definition = "testdef", namespace = "http://example.com")
  ab <- defined(1:6, label = "testlabel", unit = "meter", definition = "testdef", namespace = "http://example.com")
  expect_equal(ab, c(a, b))
  cm <- defined(4:6, label = "testlabel", unit = "centimeter", definition = "test", namespace = "http://example.com")
  def <- defined(4:6, label = "testlabel", unit = "meter", definition = "def", namespace = "http://example.com")
  nsp <- defined(4:6, label = "testlabel", unit = "meter", definition = "testdef", namespace = "http://examples.com")
  lbl <- defined(4:6, label = "tested", unit = "meter", definition = "def", namespace = "http://example.com")
  expect_error(c(a, cm),
    regexp = "must have no unit or the same unit"
  )
  expect_error(c(a, def),
    regexp = "must have no definition or the same definition"
  )
  expect_error(c(a, nsp),
    regexp = "must have no namespace or the same namespace"
  )
  expect_error(c(a, lbl),
    regexp = "must have no var_label or the same var_label"
  )
})
