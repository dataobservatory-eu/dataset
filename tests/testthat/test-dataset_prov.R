


test_that("provenance() works", {
  expect_true(is.finite.POSIXlt(provenance(iris_dataset)$started_at))
  expect_true(is.finite.POSIXlt(provenance(iris_dataset)$ended_at))
  expect_true(grepl("doi:", provenance(iris_dataset)$wasAssocitatedWith))
  expect_equal(provenance(iris_dataset)$wasInformedBy, datasource_get(iris_dataset))
})
