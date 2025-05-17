test_that("idcol_find() works", {
  expect_equal(idcol_find(orange_df, "rowid"), 1)
})

test_that("idcol_find works with valid character input", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_equal(idcol_find(df, "a"), 1)
  expect_equal(idcol_find(df, "b"), 2)
})

test_that("idcol_find works with valid numeric input", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_equal(idcol_find(df, 1), 1)
  expect_equal(idcol_find(df, 2), 2)
})

test_that("idcol_find returns error for invalid character input", {
  df <- data.frame(a = 1:3)
  expect_error(idcol_find(df, "c"), regexp = "not found")
})

test_that("idcol_find returns error for out-of-bounds numeric input", {
  df <- data.frame(a = 1:3)
  expect_error(idcol_find(df, 5), regexp = "out of bounds")
})

test_that("idcol_find returns error for wrong input type", {
  df <- data.frame(a = 1:3)
  expect_error(idcol_find(df, TRUE), "must be a column name")
  expect_error(idcol_find(df, list("a")), "must be a column name")
})

test_that("idcol_find returns error if character
          name matches multiple columns", { # creates duplicate names
  df <- data.frame(x = 1:3, x = 4:6, check.names = FALSE)
  expect_error(idcol_find(df, "x"), "exactly one column")
})

test_that("idcol_find returns correct index in real datasets", {
  mtcars2 <- mtcars
  mtcars2$rowid <- row.names(mtcars)
  expect_equal(idcol_find(mtcars2, "rowid"), ncol(mtcars2))
  expect_error(idcol_find(mtcars2, "bowid"), "not found")
})

