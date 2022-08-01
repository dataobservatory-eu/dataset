iris_dataset <- datacite_add(
  x = iris,
  Title = "Iris Dataset",
  Creator = person("Anderson", "Edgar", role = "aut"),
  Publisher= "American Iris Society",
  Identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  PublicationYear = 1935,
  Description = "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.",
  Language = "en")

a <- bibentry_dataset(iris_dataset)

test_that("bibentry_dataset() works", {
  expect_equal(class(a), 'bibentry')
})
