Cdb <- dataset_df(
  a = defined(c(11, 14, 16, 12, 17, 19), label = "length", unit = "cm"),
  dataset_bibentry = dublincore(
    title = "Test",
    creator = c(person("Jane Doe")),
    dataset_date = Sys.Date()
  )
)

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
  expect_equal(D$a, defined(c(11, 14, 16, 12, 17, 19), label = "length", unit = "cm"))
  expect_equal(dataset_title(A), dataset_title(D))
  expect_equal(get_bibentry(A)$year, get_bibentry(D)$year)
})

test_that("bind_defined_rows() works", {
  A <- dataset_df(
    a = defined(c(11, 14, 16), label = "length", unit = "cm"),
    dataset_bibentry = dublincore(
      title = "Test", creator = person("Jane Doe"),
      dataset_date = Sys.Date()
    )
  )
  expect_error(bind_defined_rows(y = A, x = data.frame()),
    regexp = "x must be a dataset_df"
  )
  expect_error(bind_defined_rows(A, y = data.frame()),
    regexp = "y must be a dataset_df"
  )
})
