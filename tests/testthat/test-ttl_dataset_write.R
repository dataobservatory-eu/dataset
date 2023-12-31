testfile <- file.path(tempdir(), "test-ttl_dataset_write.ttl")
testtdf <- data.frame ( s = c("eg:o1", "eg:01", "eg:02"),
                        p = c("a", "eg-var:", "eg-var"),
                        o = c("qb:Observation", "\"1\"^^<xs:decimal>", "\"2\"^^<xs:decimal>") )

ttl_dataset_write(tdf=testtdf,
                  ttl_namespace = NULL,
                  file_path=testfile,
                  overwrite=TRUE)

test_that("ttl_dataset_write() works:", {
  expect_true(file.exists(testfile))
  expect_true(grepl("@prefix  owl:", readLines(testfile)[1]))
  expect_equal(sum(
    vapply(readLines(testfile), function(x)
      grepl("# -- Observations --", x),
      logical(1))
    )
, 1)
})

test_that("ttl_dataset_write() validation works:", {
  expect_error(ttl_dataset_write(tdf=iris, file = tempfile()))
  expect_error(ttl_dataset_write(tdf=list(), file = tempfile()))
})
