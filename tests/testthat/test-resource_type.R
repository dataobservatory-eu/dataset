x <- data.frame()
resource_type(x) <- "Dataset"

y <- data.frame()
resource_type(y) <- "Census Data"

test_that("resource_type() works", {
  expect_equal(resource_type(y)$resourceType, "Census Data")
  expect_equal(resource_type(x)$resourceType, "Dataset")
  expect_equal(resource_type(y)$resourceTypeGeneral, "Dataset")
})
