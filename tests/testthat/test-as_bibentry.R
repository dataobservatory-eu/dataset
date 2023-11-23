myBibentry <- as_bibentry(author = c(person(family ="Anderson", given ="Edgar",
                                            role = "aut")),
                          title = "The iris Dataset",
                          datasource = "https://zenodo.org/record/7421899/files/iris.csv?download=1")


test_that("as_bibentry() internal function works", {
  expect_equal(myBibentry$title, "The iris Dataset")
  expect_equal(myBibentry$language, ":unas")
  expect_equal(myBibentry$publisher, ":tba")
  expect_equal(myBibentry$identifier, ":tba")
  expect_equal(myBibentry$version, "0.1.0")
  expect_equal(myBibentry$source, "https://zenodo.org/record/7421899/files/iris.csv?download=1")
  expect_equal(myBibentry$description, ":unas")
  expect_equal(myBibentry$year, substr(as.character(Sys.Date()),1,4))
})
