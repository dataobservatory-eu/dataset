my_ds <- dataset (data.frame(sex = c("M", "F"),
                    value = c(1,2)
                    ),
         Measures = "value",
         Dimensions = "sex")

iris_ds <- datacite_add(
   x = iris,
   Title = "Iris Dataset",
   Creator = person(family="Anderson", given ="Edgar", role = "aut"),
   Publisher = "American Iris Society",
   PublicationYear = 1935,
   Geolocation = "US",
   Language = "en")

test_that("datacite() works", {
  expect_equal(datacite(iris_ds)$Language, 'eng')
  expect_equal(datacite(iris_ds)$Creator, person ( given = "Edgar", family = "Anderson", role = "aut"))
  expect_true(grepl("KiB", attr(iris_ds, "Size")))
})
