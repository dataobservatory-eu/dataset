

a <- add_relitem(NULL, "base", "IsCompiledBy")
b <- add_relitem(RelatedIdentifier = a, "eurostat", "IsCompiledBy")

b
test_that("atttriubte is set", {
  expect_equal(a$RelatedIdentifier, "https://www.R-project.org/")
})
