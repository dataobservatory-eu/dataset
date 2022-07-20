dct_iris <- dublincore_add(
  x = iris,
  Title = "Iris Dataset",
  Creator = person("Edgar", "Anderson", role = "aut"),
  Publisher= "American Iris Society",
  Source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  Date = 1935,
  Language = "en"
)

test_that("dublincore works", {
  expect_equal(dublincore(dct_iris)$Language, 'eng')
  expect_equal(dublincore(dct_iris)$Creator, person("Edgar", "Anderson", role = "aut"))
})
