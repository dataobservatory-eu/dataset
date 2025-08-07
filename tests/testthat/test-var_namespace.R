test_that("var_namespace() retrieves the namespace correctly", {
  x <- defined("Q42",
               namespace = "https://www.wikidata.org/wiki/")
  expect_equal(var_namespace(x), "https://www.wikidata.org/wiki/")
})

test_that("var_namespace() returns NULL if no namespace is set", {
  x <- defined("Q42")
  expect_null(var_namespace(x))
})

test_that("var_namespace<-() sets namespace correctly", {
  x <- defined("Q42")
  var_namespace(x) <- "https://example.org/ns/"
  expect_equal(var_namespace(x), "https://example.org/ns/")
})

test_that("var_namespace<-() removes the namespace with NULL", {
  x <- defined("Q42", namespace = "https://example.org/ns/")
  var_namespace(x) <- NULL
  expect_null(var_namespace(x))
})

test_that("get_variable_namespaces() is an alias of var_namespace()", {
  x <- defined("Q42", namespace = "https://wikidata.org/")
  expect_equal(get_variable_namespaces(x), var_namespace(x))
})

test_that("namespace_attribute() and its setter work as expected", {
  x <- defined("Q123")
  namespace_attribute(x) <- "https://custom.org/"
  expect_equal(namespace_attribute(x), "https://custom.org/")
})

test_that("set_namespace_attribute() validates input correctly", {
  x <- defined("Q123")

  # No namespace is set
  expect_null(namespace_attribute(x), "https://valid.org/")

  # Valid input
  expect_no_error(set_namespace_attribute(x, "https://valid.org/"))
  x <- (set_namespace_attribute(x, "https://valid.org/"))
  expect_equal(var_namespace(x),  "https://valid.org/")

  # NULL input removes namespace
  x <- set_namespace_attribute(x, NULL)
  expect_null(namespace_attribute(x))

  # Invalid: non-character
  expect_error(
    set_namespace_attribute(x, 123),
    regexp = "`namespace` should be a single character string or NULL"
  )

  # Invalid: multiple strings
  expect_error(
    set_namespace_attribute(x, c("ns1", "ns2")),
    regexp = "`namespace` should be a single character string or NULL"
  )
})

test_that("namespace_attribute<-() uses set_namespace_attribute", {
  x <- defined("Q42")
  x <- `namespace_attribute<-`(x, "https://ns.org/")
  expect_equal(namespace_attribute(x), "https://ns.org/")
})
