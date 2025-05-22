test_that("labelled_defined() works", {
  z <- defined(c(1, 1, 1, 0, 0, 0),
    label = "",
    labels = c("F" = 0, "M" = 1, "_N" = 99),
    concept = "https://registry.sdmx.org/sdmx/v2/structure/codelist/SDMX/CL_SEX/2.1#"
  )
  x <- defined(c(0, 1, 0, 1, 1, 0),
    label = "sex",
    labels = c("F" = 0, "M" = 1, "_N" = 99),
    concept = "https://registry.sdmx.org/sdmx/v2/structure/codelist/SDMX/CL_SEX/2.1#"
  )
  v <- defined(c(1, 0),
    label = "sex",
    labels = c("F" = 0, "M" = 1, "_N" = 99),
    concept = "https://registry.sdmx.org/sdmx/v2/structure/codelist/SDMX/CL_SEX/2.1#"
  )
  y <- defined(c(1, 1, 1, 0, 0, 0),
    label = "sex",
    labels = c("F" = 0, "M" = 1, "_N" = 99),
    concept = "https://registry.sdmx.org/sdmx/v2/structure/codelist/SDMX/CL_SEX/2.1#"
  )
  expect_equal(c(1:3, y), c(1, 2, 3, 1, 1, 1, 0, 0, 0))
  expect_equal(
    c("a", "b", y),
    c("a", "b", as.character(c(1, 1, 1, 0, 0, 0)))
  )
  expect_equal(
    c(x, y),
    defined(c(0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0),
      label = "sex",
      labels = c("F" = 0, "M" = 1, "_N" = 99),
      concept = "https://registry.sdmx.org/sdmx/v2/structure/codelist/SDMX/CL_SEX/2.1#"
    )
  )
  expect_equal(
    c(x, y, v),
    defined(c(0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0),
      label = "sex",
      labels = c("F" = 0, "M" = 1, "_N" = 99),
      concept = "https://registry.sdmx.org/sdmx/v2/structure/codelist/SDMX/CL_SEX/2.1#"
    )
  )
  a <- defined(c("a", "b"), label = "c")
  d <- defined(c("d", "e"), label = "c")
  expect_equal(
    c(a, d),
    defined(c("a", "b", "d", "e"),
      label = "c"
    )
  )
})

test_that("labelled_defined() works", {
  sepal_length <- defined(iris$Sepal.Length,
    labels = NULL,
    label = "Sepal length",
    unit = "centimeters",
    concept = "https://www.wikidata.org/wiki/Property:P2043"
  )
  myspecies <- defined(
    x = iris$Species,
    label = "Taxon name within the Iris genus",
    concept = "https://npgsweb.ars-grin.gov/gringlobal/taxon/taxonomygenus?id=6074",
    namespace = "Iris"
  )
  expect_equal(is.defined(sepal_length), TRUE)
  expect_equal(var_label(sepal_length), "Sepal length")
  expect_equal(var_unit(sepal_length), "centimeters")
  expect_equal(var_concept(sepal_length), "https://www.wikidata.org/wiki/Property:P2043")
  expect_equal(var_namespace(myspecies), "Iris")
  expect_true(all(as.character(sepal_length) == as.character(iris$Sepal.Length)))
})


test_that("Subsetting defined vectors works correctly", {
  v <- defined(10:20,
    label = "Test Vector",
    unit = "kg",
    concept = "http://example.com/def",
    namespace = "http://example.com/ns"
  )

  sub_v <- v[1:5]
  expect_true(is.defined(sub_v))
  expect_equal(var_label(sub_v), "Test Vector")
  expect_equal(var_unit(sub_v), "kg")
  expect_equal(length(sub_v), 5)

  single <- v[[3]]
  expect_equal(as_numeric(single), 12)
  expect_equal(var_label(single), "Test Vector")
  expect_equal(var_unit(single), "kg")
})

test_that("[[.haven_labelled_defined returns a scalar defined value with metadata", {
  x <- defined(
    c(10, 20, 30),
    label = "Measurement",
    unit = "cm",
    concept = "http://example.org/def",
    namespace = "http://example.org/ns",
    labels = c("Low" = 10, "Medium" = 20, "High" = 30)
  )

  val <- x[[2]]

  expect_true(is.defined(val))
  expect_equal(as_numeric(val), 20)
  expect_equal(var_label(val), "Measurement")
  expect_equal(var_unit(val), "cm")
  expect_equal(var_concept(val), "http://example.org/def")
  expect_equal(var_namespace(val), "http://example.org/ns")

  # Check that labels are preserved too
  expect_equal(attr(val, "labels"), c("Low" = 10, "Medium" = 20, "High" = 30))
})

test_that("Comparison operations work on defined vectors", {
  a <- defined(1:5, label = "Test", unit = "x", concept = "def")
  b <- defined(5:1, label = "Test", unit = "x", concept = "def")

  expect_equal(a == b, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(a < 3, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(3 > b, c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(a != b, c(TRUE, TRUE, FALSE, TRUE, TRUE))
})


test_that("length, head, and tail work on defined vectors", {
  x <- defined(1:10, label = "Demo", unit = "m", concept = "test")

  expect_equal(length(x), 10)
  expect_equal(length(head(x, 3)), 3)
  expect_equal(length(tail(x, 2)), 2)
  expect_equal(as_numeric(head(x, 2)), c(1, 2))
  expect_equal(as_numeric(tail(x, 3)), c(8, 9, 10))

  expect_true(is.defined(head(x, 3)))
  expect_true(is.defined(tail(x, 2)))
  expect_equal(var_label(head(x, 3)), "Demo")
  expect_equal(var_unit(tail(x, 2)), "m")
  expect_equal(var_concept(head(x, 1)), "test")
})

test_that("print.haven_labelled_defined outputs correctly with/without definition and unit", {
  x1 <- defined(1:3, concept = "https://def", unit = "kg")
  x2 <- defined(1:3, concept = "https://def")
  x3 <- defined(1:3, unit = "kg")
  x4 <- defined(1:3)

  expect_output(print(x1), "Defined as https://def, measured in kg")
  expect_output(print(x2), "Defined as https://def")
  expect_output(print(x3), "Measured in kg")
  expect_output(print(x4), "Defined vector")
})

test_that("format.haven_labelled_defined works correctly", {
  x <- defined(1:3, unit = "kg", concept = "https://def")
  expect_equal(format(x), c("1 (kg)", "2 (kg)", "3 (kg)"))

  y <- defined(4:6, concept = "short-def")
  expect_equal(format(y), c("4 [short-def]", "5 [short-def]", "6 [short-def]"))

  z <- defined(7:9)
  expect_equal(format(z), c("7", "8", "9"))
})


test_that("as.vector.haven_labelled_defined works correctly", {
  x <- defined(1:3, unit = "kg", concept = "http://example.com")
  expect_equal(as.vector(x), c(1, 2, 3))
  expect_equal(as.vector(x, mode = "character"), c("1", "2", "3"))

  y <- defined(c("a", "b"))
  expect_equal(as.vector(y), c("a", "b"))
})

test_that("as.list.haven_labelled_defined preserves metadata", {
  x <- defined(1:2, label = "Test", unit = "kg", concept = "def")
  lst <- as.list(x)
  expect_length(lst, 2)
  expect_true(all(vapply(lst, is.defined, logical(1))))
  expect_equal(var_unit(lst[[1]]), "kg")
})

test_that("labelled_defined() throws error", {
  expect_error(var_unit(sepal_length) <- c("cm", "mm"))
  expect_error(var_unit(sepal_length) <- 1)
  expect_error(defined(
    x = iris$Species,
    label = "Taxon name within the Iris genus",
    definition = 1,
    concept = "Iris"
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
    concept = "Definition",
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
    concept = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = c(1:3),
    label = "Numbers",
    concept = 1,
    namespace = "Iris"
  ))
  expect_error(defined(
    x = c(1:3),
    label = "Numbers",
    concept = 1,
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


test_that("c() works", {
  a <- defined(iris$Sepal.Length[1:3],
    labels = NULL,
    label = "Sepal length",
    unit = "centimeters",
    concept = "https://www.wikidata.org/wiki/Property:P2043"
  )


  b <- defined(iris$Sepal.Length[4:6],
    labels = NULL,
    label = "Sepal length",
    unit = "centimeters",
    concept = "https://www.wikidata.org/wiki/Property:P2043"
  )

  bmm <- defined(iris$Sepal.Length[7:9] * 10,
    labels = NULL,
    label = "Sepal length",
    unit = "milimeters",
    concept = "https://www.wikidata.org/wiki/Property:P2043"
  )
  expect_equal(is.defined(c(a, b)), TRUE)
  expect_equal(length(c(a, b)), 6)
  expect_error(c(a, bmm))
})


test_that("summary.haven_labelled_defined() works ", {
  sepal_length <- iris_dataset$Sepal.Length
  expect_output(summary(sepal_length), "Length of the sepal in cm \\(centimeter\\)")
  expect_equal(names(summary(sepal_length))[1], "Min.")
})


test_that("summary() produces no output when label/unit are missing", {
  x <- defined(1:5)
  expect_silent(summary(x)) # expect no printed title
})


test_that("as_numeric() returns underlying numeric vector", {
  x <- defined(1:3, label = "Test", unit = "kg")
  expect_equal(as_numeric(x, preserve_attributes = FALSE), c(1, 2, 3))
  expect_equal(attr(as_numeric(x, TRUE), "unit"), "kg")
  expect_type(as_numeric(x), "integer")
})

test_that("as_numeric() returns underlying numeric vector", {
  x <- defined(1:3, label = "Test", unit = "kg")
  expect_equal(as.numeric(x), c(1, 2, 3))
})

test_that("as_character() returns underlying character vector", {
  fruits <- defined(c("apple","avocado", "kiwi"), label = "Fruit", unit = "kg")
  expect_equal(as_character(fruits, preserve_attributes = FALSE), c("apple","avocado", "kiwi"))
  expect_equal(attr(as_character(fruits, TRUE), "unit"), "kg")
  expect_type(as_character(fruits), "character")
  expect_error(as_numeric(fruits), regexp = "underlying data is not numeric")
})

test_that("as_factor() works with defined vector", {
  x <- defined(
    c(0, 1, 1, 0),
    label = "Sex",
    labels = c("Female" = 0, "Male" = 1)
  )
  f <- as_factor(x)
  expect_s3_class(f, "factor")
  expect_equal(levels(f), c("Female", "Male"))
  expect_equal(as.character(f), c("Female", "Male", "Male", "Female"))
})


test_that("c.haven_labelled_defined() works ", {
  a <- defined(1:3, label = "testlabel", unit = "meter", concept = "testdef", namespace = "http://example.com")
  b <- defined(4:6, label = "testlabel", unit = "meter", concept = "testdef", namespace = "http://example.com")
  ab <- defined(1:6, label = "testlabel", unit = "meter", concept = "testdef", namespace = "http://example.com")
  expect_equal(ab, c(a, b))
  cm <- defined(4:6, label = "testlabel", unit = "centimeter", concept = "test", namespace = "http://example.com")
  def <- defined(4:6, label = "testlabel", unit = "meter", concept = "def", namespace = "http://example.com")
  nsp <- defined(4:6, label = "testlabel", unit = "meter", concept = "testdef", namespace = "http://examples.com")
  lbl <- defined(4:6, label = "tested", unit = "meter", concept = "def", namespace = "http://example.com")
  expect_error(c(a, cm),
    regexp = "must have no unit or the same unit"
  )
  expect_error(c(a, def),
    regexp = "must have no concept definition or the same concept definition"
  )
  expect_error(c(a, nsp),
    regexp = "must have no namespace or the same namespace"
  )
  expect_error(c(a, lbl),
    regexp = "must have no var_label or the same var_label"
  )
})
