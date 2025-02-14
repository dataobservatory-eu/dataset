test_that("datacite() works", {
  iris_datacite <- datacite(
    Title = "Iris Dataset",
    Creator = person(family = "Anderson", given = "Edgar", role = "aut"),
    Publisher = "American Iris Society",
    PublicationYear = 1935,
    Geolocation = "US",
    Language = "en"
  )
  expect_true(is.datacite(iris_datacite))
  expect_equal(iris_datacite$language, "en")
  expect_equal(iris_datacite$geolocation, "US")
  expect_equal(iris_datacite$rights, ":tba")
  expect_equal(iris_datacite$subject, "data sets")
})

test_that("as_datacite() works", {
  expect_type(as_datacite(iris_dataset, "list"), "list")
  expect_true(is.dataset_df(as_datacite(iris_dataset, "dataset_df")))
  expect_warning(as_datacite(x = iris_dataset, type = "data.frame"))
  expect_equal(as_datacite(iris_dataset, "list")$FundingReference, ":unas")
  expect_equal(as_datacite(iris_dataset, "list")$Description, "The famous (Fisher's or Anderson's) iris data set.")
})
