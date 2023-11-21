iris_datacite <- datacite(
   Title = "Iris Dataset",
   Creator = person(family="Anderson", given ="Edgar", role = "aut"),
   Publisher = "American Iris Society",
   PublicationYear = 1935,
   Geolocation = "US",
   Language = "en")

test_that("datacite() works", {
  expect_true(is.datacite(iris_datacite))
  expect_equal(iris_datacite$language, 'en')
  expect_equal(iris_datacite$geolocation, 'US')
  expect_equal(iris_datacite$rights, ':tba')
})
