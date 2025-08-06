test_that("var_label() works with defined() objects", {
  x <- defined(1:5, label = "Defined label")
  expect_equal(var_label(x), "Defined label")

  var_label(x) <- "Updated label"
  expect_equal(var_label(x), "Updated label")
})

test_that("var_label<- works with haven_labelled_defined", {
  x <- haven::labelled(1:5, labels = c(a = 1, b = 2))
  x <- defined(x)
  var_label(x) <- "Labelled defined"
  expect_equal(var_label(x), "Labelled defined")
})

test_that("var_label.dataset_df returns expected label list", {
  df <- dataset_df(
    x = defined(1:3, label = "Label X"),
    y = defined(4:6),
    z = defined(letters[1:3], label = "Label Z")
  )

  expect_equal(var_label(df)$x, "Label X")
  expect_null(var_label(df)$y)
  expect_equal(var_label(df)$z, "Label Z")
})

test_that("var_label.dataset_df supports null_action options", {
  df <- dataset_df(a = defined(1:2, label = "Has Label"), b = 3:4)

  expect_equal(var_label(df, null_action = "fill")$b, "b")
  expect_true(is.na(var_label(df, null_action = "na")$b))
  expect_equal(var_label(df, null_action = "empty")$b, "")
  expect_null(var_label(df, null_action = "keep")$b)

  skipped <- var_label(df, null_action = "skip")
  expect_false("b" %in% names(skipped))
})

test_that("var_label.dataset_df returns named vector with unlist = TRUE", {
  df <- dataset_df(
    u = defined(1:3, label = "U label"),
    v = 4:6
  )
  out <- var_label(df, unlist = TRUE, null_action = "empty")
  expect_type(out, "character")
  expect_equal(out[["u"]], "U label")
  expect_equal(out[["v"]], "")
})

test_that("var_label.dataset_df handles recurse = TRUE on packed columns", {
  packed <- dataset_df(
    a = defined(1:3, label = "A"),
    b = tibble::tibble(x = defined(1:3, label = "SubX"), y = 4:6)
  )

  out <- var_label(packed, recurse = TRUE, null_action = "empty")
  expect_type(out$b, "list")
  expect_equal(out$b$x, "SubX")
})


test_that("var_label() null_action options on dataset_df", {
  df <- dataset_df(x = defined(1:3, label = "has label"), y = 4:6)

  expect_equal(var_label(df, null_action = "keep")$y, NULL)
  expect_equal(var_label(df, null_action = "fill")$y, "y")
  expect_equal(var_label(df, null_action = "skip")$y, NULL)
  expect_true(is.na(var_label(df, null_action = "na")$y))
  expect_equal(var_label(df, null_action = "empty")$y, "")
})

test_that("var_label() with unlist = TRUE returns named vector", {
  df <- dataset_df(x = defined(1:3, label = "x label"), y = defined(4:6))
  out <- var_label(df, unlist = TRUE, null_action = "empty")
  expect_type(out, "character")
  expect_named(out, c("rowid", "x", "y"))
  expect_equal(out[["x"]], "x label")
  expect_equal(out[["y"]], "")
})

test_that("label_attribute() returns exact label attribute or NULL", {
  x <- 1:5
  expect_null(label_attribute(x))

  attr(x, "label") <- "simple label"
  expect_equal(label_attribute(x), "simple label")
})

test_that("var_label<- removes label when value is NULL", {
  x <- defined(1:3)
  var_label(x) <- "temporary label"
  expect_equal(var_label(x), "temporary label")
  var_label(x) <- NULL
  expect_null(var_label(x))
})

test_that("set_var_labels() assigns multiple labels to a dataset", {
  df <- data.frame(a = 1:3, b = 4:6)
  lbls <- list(a = "Label A", b = "Label B")
  out <- set_var_labels(df, lbls)

  expect_equal(attr(out, "var_labels")$a, "Label A")
  expect_equal(attr(out, "var_labels")$b, "Label B")
})

