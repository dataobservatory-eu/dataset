
iris_dataset <- language_add(x = iris, Language= "English")

test_that("language", {
  expect_equal(language(iris_dataset), "eng")
})
