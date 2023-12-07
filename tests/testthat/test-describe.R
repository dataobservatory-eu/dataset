
iris_ds <- dataset(
  x = iris,
  title = "Iris Dataset",
  author = person("Edgar", "Anderson", role = "aut"),
  publisher = "American Iris Society",
  source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en",
  description = "This famous (Fisher's or Anderson's) iris data set."
)

example_ds <- dataset(x = data.frame(a=1, b=2),
                      author = person("Joe", "Doe"),
                      title = "Example dataset")


test_that("describe() works", {
  expect_output(describe(iris_ds))
  expect_output(describe(x=example_ds))
})
