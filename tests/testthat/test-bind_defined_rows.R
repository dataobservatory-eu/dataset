test_that("bind_defined_rows() works", {
  A <- dataset_df(
    a = defined(c(11, 14, 16), label = "length", unit = "cm"),
    dataset_bibentry = dublincore(
      title = "Test", creator = person("Jane Doe"),
      dataset_date = Sys.Date()
    )
  )
  B <- dataset_df(
    a = defined(c(12, 17, 19), label = "length", unit = "cm"),
    dataset_bibentry = dublincore(
      title = "Test", creator = person("Jane Doe")
    )
  )
  D <- bind_defined_rows(x = A, y = B)
  expect_equal(as_numeric(D$a), c(11, 14, 16, 12, 17, 19))
  expect_equal(D$a, defined(c(11, 14, 16, 12, 17, 19),
    label = "length", unit = "cm"
  ))
  expect_equal(dataset_title(A), dataset_title(D))
  expect_equal(get_bibentry(A)$year, get_bibentry(D)$year)
})

test_that("bind_defined_rows() detects if not dataset_df is
          the input parameter", {
  A <- dataset_df(
    a = defined(c(11, 14, 16),
      label = "length",
      unit = "cm"
    ),
    dataset_bibentry = dublincore(
      title = "Test", creator = person("Jane Doe"),
      dataset_date = Sys.Date()
    )
  )
  expect_error(
    bind_defined_rows(
      y = A,
      x = data.frame()
    ),
    regexp = "x\\` must be a dataset_df object"
  )
  expect_error(
    bind_defined_rows(A,
      y = data.frame()
    ),
    regexp = "y\\` must be a dataset_df"
  )
})

test_that("bind_defined_rows() detects different labels", {
  A <- dataset_df(
    width = defined(c(1, 2), label = "width", unit = "cm"),
    height = defined(c(3, 4), label = "height", unit = "cm")
  )
  B <- dataset_df(
    width = defined(c(3, 4), unit = "cm"),
    height = defined(c(2, 2), label = "height", unit = "cm")
  )
  expect_error(bind_defined_rows(x = A, y = B),
    regexp = "Variable labels"
  )
})

test_that("bind_defined_rows() detects different units", {
  A <- dataset_df(
    width = defined(c(1, 2), label = "width", unit = "cm"),
    height = defined(c(3, 4), label = "height", unit = "mm")
  )
  B <- dataset_df(
    width = defined(c(3, 4), label = "width", unit = "cm"),
    height = defined(c(2, 2), label = "height", unit = "cm")
  )
  expect_error(bind_defined_rows(x = A, y = B),
    regexp = "Variable units"
  )
})

test_that("bind_defined_rows() detects different definitions", {
  A <- dataset_df(
    width = defined(c(1, 2), label = "width", concept = "cm"),
    height = defined(c(3, 4), label = "height", concept = "mm")
  )
  B <- dataset_df(
    width = defined(c(3, 4), label = "width", concept = "cm"),
    height = defined(c(2, 2), label = "height", concept = "cm")
  )
  expect_error(bind_defined_rows(x = A, y = B),
    regexp = "Variable concept definitions"
  )
})


test_that("bind_defined_rows() detects different namespace", {
  A <- dataset_df(
    width = defined(c(1, 2),
      label = "width",
      unit = "cm", namespace = "http://example.com"
    ),
    height = defined(c(3, 4),
      label = "height",
      unit = "cm", namespace = "http://example.com"
    )
  )
  B <- dataset_df(
    width = defined(c(3, 4),
      label = "width",
      unit = "cm", namespace = "http://example.com"
    ),
    height = defined(c(2, 2), label = "height", unit = "cm")
  )
  expect_error(bind_defined_rows(x = A, y = B),
    regexp = "Variable namespaces"
  )
})

test_that("bind_defined_rows() has the same rowid namespace", {
  A <- dataset_df(
    width = defined(c(1, 2),
      label = "width",
      unit = "cm", namespace = "http://example.com"
    ),
    height = defined(c(3, 4),
      label = "height",
      unit = "cm", namespace = "http://example.com"
    ),
    identifier = c(wbi = "https:://example.com/")
  )
  B <- dataset_df(
    width = defined(c(3, 4),
      label = "width",
      unit = "cm", namespace = "http://example.com"
    ),
    height = defined(c(2, 2),
      label = "height",
      unit = "cm", namespace = "http://example.com"
    ),
    identifier = c(wbi = "https:://example.com/")
  )
  D <- dataset_df(
    width = defined(c(3, 4),
      label = "width",
      unit = "cm", namespace = "http://example.com"
    ),
    height = defined(c(2, 2),
      label = "height",
      unit = "cm", namespace = "http://example.com"
    ),
    identifier = c(wbi = "https:://example.net/")
  )
  expect_equal(
    namespace_attribute(bind_defined_rows(x = A, y = B)$rowid),
    namespace_attribute(B$rowid)
  )
  expect_error(bind_defined_rows(x = A, y = D, strict = TRUE),
    regexp = "Row identifier namespaces must match"
  )
})
