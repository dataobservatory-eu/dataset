myiris <- iris
converted <- as_dataset(x=iris,
                        author = c(person(family ="Anderson", given ="Edgar",
                                          role = "aut")),
                        title = "The iris Dataset",
                        datasource = "https://zenodo.org/record/7421899/files/iris.csv?download=1")


test_that("as_dataset method works", {
  expect_true(is.dataset(converted))
  expect_equal(class(attr(converted, "DataBibentry")), "bibentry")
  expect_equal(attr(converted, "DataBibentry")$author,
               c(person(family ="Anderson",
                        given ="Edgar",
                        role = "aut"))
  )
  expect_equal(attr(converted, "DataBibentry")$title,
               "The iris Dataset")
  expect_equal(attr(converted, "DataBibentry")$resourcetype,
               "Dataset")
  expect_equal(attr(converted, "DataBibentry")$source,
               "https://zenodo.org/record/7421899/files/iris.csv?download=1")
})
