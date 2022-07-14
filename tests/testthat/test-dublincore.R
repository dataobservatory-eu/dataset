dct_iris <- dublincore_add(
  x = iris,
  title = "Iris Dataset",
  creator = person("Edgar", "Anderson", role = "aut"),
  publisher= "American Iris Society",
  source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en"
)

test_that("dublincore works", {
  expect_equal(dublincore(dct_iris)$language, 'eng')
  expect_equal(dublincore(dct_iris)$creator, person("Edgar", "Anderson", role = "aut"))
})
