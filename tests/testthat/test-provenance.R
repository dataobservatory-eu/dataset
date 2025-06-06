test_that("provenance(x) works", {
  expect_error(provenance(mtcars),
    regexp = "must be a dataset_df object with standardised provenance metadata"
  )
  tested <- dataset_df(x = mtcars)
  expect_true(class(provenance(iris_dataset)[1]) == "character")
  expect_output(print(provenance(tested)), "<http://example.com/dataset#>")
  provenance(x = tested) <- n_triple("https://doi.org/10.5281/zenodo.10396807", "http://www.w3.org/ns/prov#wasInformedBy", "http://example.com/source#1")
  expect_output(print(provenance(tested)), "<http://example.com/source#1>")
})
