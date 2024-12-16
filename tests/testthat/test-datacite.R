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

test_that("as_datacite() works", {
  expect_true(is.datacite(as_datacite(iris_dataset)))
  expect_true(is.list(as_datacite(iris_dataset, type="list")))
  expect_equal(as_datacite(iris_dataset, type="list")$Publisher, 'American Iris Society')
  expect_true(is.dataset_df(as_datacite(iris_dataset, type="dataset")))
  expect_equal(dataset_title(as_datacite(iris_dataset, type="dataset")), "The DataCite Metadata of `Iris Dataset'")
  expect_equal(creator(as_datacite(iris_dataset, type ="dataset", author = person("Jane", "Doe"))),
            person("Jane", "Doe"))
})

test_that("as_datacite() gives warning", {
  expect_warning(as_datacite(iris_dataset, type = "character"))
})
