test_that("id_to_column() works on data.frame", {
  df <- iris

  # Default behavior (prefix = "eg:")
  df_with_ids <- id_to_column(df)
  expect_true("rowid" %in% names(df_with_ids))
  expect_equal(df_with_ids$rowid[1], "eg:1")

  # Custom prefix
  df_with_custom_prefix <- id_to_column(df, prefix = "iris:")
  expect_equal(df_with_custom_prefix$rowid[3], "iris:3")

  # No prefix
  df_with_no_prefix <- id_to_column(df, prefix = NULL)
  expect_equal(df_with_no_prefix$rowid[1], "1")

  # Custom IDs
  ids <- paste0("iris-record-", seq_len(nrow(df)))
  df_with_custom_ids <- id_to_column(df, prefix = "ex:", ids = ids)
  expect_equal(df_with_custom_ids$rowid[1], "ex:iris-record-1")

  # Error on bad ID length
  expect_error(id_to_column(df, ids = c("a", "b")), "ids must be of same lengths")
})

test_that("id_to_column() works on dataset_df and preserves metadata", {
  creator_person <- person(given = "Jane", family = "Doe", role = "aut")

  original <- dataset_df(
    mtcars,
    dataset_bibentry = dublincore(
      title = "MTCars Dataset",
      creator = creator_person,
      publisher = "Motor Trend",
      dataset_date = 1974
    )
  )

  out <- id_to_column(original, prefix = "mt:")

  expect_s3_class(out, "dataset_df")
  expect_true("rowid" %in% names(out))

  # Bibentry preserved
  b <- get_bibentry(out)
  expect_s3_class(b, "bibentry")
  expect_equal(b$title, "MTCars Dataset")
  expect_equal(b$author, creator_person)

  # Provenance and subject preserved
  expect_identical(subject(out), subject(original))
  expect_identical(provenance(out), provenance(original))
})

test_that("id_to_column() overwrites existing rowid safely", {
  df <- data.frame(a = 1:3, rowid = letters[1:3])
  out <- id_to_column(df, prefix = "new:")
  expect_equal(out$rowid, paste0("new:", 1:3))
})
