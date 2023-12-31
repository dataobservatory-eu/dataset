testfile <- file.path(tempdir(), "test-dataset_ttl_write.ttl")
testtdf <- data.frame ( s = c("eg:o1", "eg:01", "eg:02"),
                        p = c("a", "eg-var:", "eg-var"),
                        o = c("qb:Observation", "\"1\"^^<xs:decimal>", "\"2\"^^<xs:decimal>") )

dataset_ttl_write(tdf=testtdf,
                  ttl_namespace = NULL,
                  file_path=testfile,
                  overwrite=TRUE)

test_that("dataset_ttl_write() works:", {
  expect_true(file.exists(testfile))
  expect_true(grepl("@prefix  owl:", readLines(testfile)[1]))
  expect_equal(sum(
    vapply(readLines(testfile), function(x)
      grepl("# -- Observations --", x),
      logical(1))
    )
, 1)
})

test_that("dataset_ttl_write() validation works:", {
  expect_error(dataset_ttl_write(tdf=iris, file = tempfile()))
  expect_error(dataset_ttl_write(tdf=list(), file = tempfile()))
})
