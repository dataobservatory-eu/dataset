
dont_test <- function() {
  a <- add_relitem(NULL, "base", "IsCompiledBy")
  b <- add_relitem(RelatedIdentifier = a, "eurostat", "IsCompiledBy")


  test_that("atttriubte is set", {
    expect_equal(a$RelatedIdentifier, "https://www.R-project.org/")
  })

}


test_that("Size atttriubte is set", {
  expect_equal(attr(size_add(iris), "Size"), "7.26 kB [7.09 KiB]")
})

test_that("Version atttriubte is set", {
  expect_equal(attr(version_add(x = iris, version= "1.0"), "Version"), "1.0")
})


test_that("Language atttriubte is set", {
  expect_equal(attr(language_add(x = iris, language= "hungarian", iso_639_code = "639-1"), "language"), "hu")
  expect_equal(attr(language_add(x = iris, language= "English"), "language"), "eng")
  expect_equal(attr(language_add(x = iris, language= "nl", iso_639_code = "639-1"), "language"), "nl")
})

