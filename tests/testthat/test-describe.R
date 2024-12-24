
temp_prov <- tempfile()
describe(iris_dataset, temp_prov)

test_that("describe() works", {
  expect_equal(readLines(temp_prov)[1], "<http://example.com/dataset_prov.nt> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Bundle> ."
)
})
