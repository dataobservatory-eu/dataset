dct_iris <- dublincore(
  title = "Iris Dataset",
  creator = person("Edgar", "Anderson", role = "aut"),
  publisher = "American Iris Society",
  source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en"
)

str(dct_iris)

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
})



