dct_iris <- dublincore(
  title = "Iris Dataset",
  creator = person("Edgar", "Anderson", role = "aut"),
  publisher = "American Iris Society",
  source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en",
  description = "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
)


test_that("dublincore works", {
  expect_true(is.dublincore(dct_iris))
})


test_that("dublincore works", {
  expect_equal(dct_iris$language, 'en')
  expect_equal(dct_iris$publisher, "American Iris Society")
  expect_equal(dct_iris$date, "1935")
  expect_equal(dct_iris$source, "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
  expect_equal(dct_iris$identifier, ":tba")
  expect_equal(dct_iris$rights, ":tba")
  expect_equal(dct_iris$author, person("Edgar", "Anderson", role = "aut"))
  expect_equal(dct_iris$type, "DCMITYPE:Dataset")
  expect_equal(dct_iris$description, "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.")
})



