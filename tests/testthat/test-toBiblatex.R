
test_that("toBibtex() imported method", {
  expect_true(inherits(toBibtex(
    object = as_dublincore(iris_dataset),
    note.replace.field = c("urldate", "pubsate", "addendum"),
    extra.fields = NULL
  ), "Bibtex"))
})

test_that("toBiblatex() imported function", {
  tested <- toBiblatex(
    object = as_dublincore(iris_dataset),
    note.replace.field = c("urldate", "pubsate", "addendum"),
    extra.fields = NULL
  )
  expect_true(inherits(tested, "Bibtex"))
})
