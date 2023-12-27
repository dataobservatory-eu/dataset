

test_that("get_prefix() works", {
  expect_equal(get_prefix("eg:refArea"), "eg:")
  expect_equal(get_prefix("eg:refArea:"), "eg:")
  expect_equal(get_prefix("[eg:refArea]"), "eg:")
  expect_equal(get_resource_identifier("eg:refArea"), "refArea")
  expect_equal(get_resource_identifier("[eg:refArea]"), "refArea")
})

