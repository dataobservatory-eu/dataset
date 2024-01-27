

test_that("provenance() works", {
  expect_true(inherits(provenance(iris_dataset)$started_at, "POSIXct"))
  expect_true(inherits(provenance(iris_dataset)$ended_at, "POSIXct"))
  expect_true(grepl("doi:", provenance(iris_dataset)$wasAssocitatedWith))
  expect_equal(provenance(iris_dataset)$wasInformedBy, datasource_get(iris_dataset))
})
