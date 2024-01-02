y <- dataset (data.frame ( a = 1:3,
                           b = 5:7),
              title = "Example dataset",
              author = person("Jane", "Doe"),
              publisher = "Publishing Co.",
              issued = as.Date("2022-07-14")
              )


test_that("dataset() works", {
  expect_true(is.dataset(y))
  expect_equal(names(y), c("a", "b"))
  expect_equal(var_labels(y), c( a="", b=""))
  expect_true(grepl("doi:", provenance(y)$wasAssocitatedWith))
  expect_true(is.finite.POSIXlt(provenance(y)$started_at))
})

example_ds <- dataset(x = data.frame(a=1, b=2),
                      author = person("Joe", "Doe"),
                      title = "Example dataset")


test_that("is.dataset() works", {
  expect_true(is.dataset(example_ds))
})

z <- set_var_labels(y, c( a="Example 1", b="Example 2"))

test_that("dataset() works", {
  expect_equal(var_labels(z), c(a="Example 1", b="Example 2"))
})

test_that("dataset() needs a title:", {
  expect_error(dataset (data.frame ( a = 1:3,
                                     b = 5:7)) )
})

