test_that("describe() works", {
  temporary_con <- tempfile()
  expect_null(describe(orange_df, temporary_con)) # not returned
  expect_equal(readLines(temporary_con)[1],
               "<http://example.com/dataset_prov.nt> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Bundle> .")
  expect_error(describe(list(a=1)),
               regexp = "x most be a a dataset_df object")
  expect_equal(describe(orange_df)[1],
               "<http://example.com/dataset_prov.nt> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Bundle> .")
})





