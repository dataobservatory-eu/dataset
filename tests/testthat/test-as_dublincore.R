
test_that("as_dublincore returns a dublincore (bibentry) object by default", {
  dc <- as_dublincore(orange_df)
  expect_s3_class(dc, "dublincore")
  expect_s3_class(dc, "bibentry")
  expect_equal(dc$title, "Growth of Orange Trees")
  expect_true(inherits(dc$author, "person") || all(vapply(dc$author, inherits, logical(1), "person")))
})

test_that("as_dublincore(type = 'list') returns a named list with correct fields", {
  dc_list <- as_dublincore(orange_df, type = "list")
  expect_type(dc_list, "list")
  expect_equal(dc_list$title, "Growth of Orange Trees")
  expect_equal(dc_list$type, "DCMITYPE:Dataset")
  expect_true("creator" %in% names(dc_list))
  expect_true("identifier" %in% names(dc_list))
})

test_that("as_dublincore(type = 'dataset_df') returns a dataset_df", {
  dc_df <- as_dublincore(orange_df, type = "dataset_df")
  expect_s3_class(dc_df, "dataset_df")
  expect_true(is.data.frame(dc_df))
  expect_equal(ncol(dc_df), 16)  # all metadata fields included
})

test_that("as_dublincore(type = 'ntriples') returns valid N-Triples syntax", {
  dc_nt <- as_dublincore(orange_df, type = "ntriples")
  expect_type(dc_nt, "character")
  expect_true(any(grepl("http://purl.org/dc/terms/title", dc_nt)))
  expect_true(any(grepl("http://purl.org/dc/terms/creator", dc_nt)))
})
