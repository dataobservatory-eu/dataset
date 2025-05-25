test_that("as_datacite returns a datacite (bibentry) object by default", {
  dc <- as_datacite(orange_df)
  expect_s3_class(dc, "datacite")
  expect_s3_class(dc, "bibentry")
  expect_equal(dc$title, "Growth of Orange Trees")
  expect_true(inherits(dc$author, "person") || all(vapply(dc$author, inherits, logical(1), "person")))
})

test_that("as_datacite(type = 'list') returns a named list with correct fields", {
  dc_list <- as_datacite(orange_df, type = "list")
  expect_type(dc_list, "list")
  expect_equal(dc_list$Title, "Growth of Orange Trees")
  expect_equal(dc_list$Type, "Dataset")
  expect_true("Creator" %in% names(dc_list))
  expect_true("Identifier" %in% names(dc_list))
  expect_true("Language" %in% names(dc_list))
})

test_that("as_datacite(type = 'dataset_df') returns a clean dataset_df", {
  datacite_df <- as_datacite(orange_df, type = "dataset_df")
  expect_s3_class(datacite_df, "dataset_df")
  expect_equal(nrow(datacite_df), 1)
  expect_true(is.character(datacite_df$Creator))
  expect_type(datacite_df$Description, "character")
})

test_that("as_datacite(type = 'ntriples') returns valid RDF triples", {
  dc_nt <- as_datacite(orange_df, type = "ntriples")
  expect_type(dc_nt, "character")
  expect_true(any(grepl("http://datacite.org/schema/kernel-4/title", dc_nt)))
  expect_true(any(grepl("http://datacite.org/schema/kernel-4/creator", dc_nt)))
})
