dct_iris <- dublincore(
  title = "Iris Dataset",
  creator = person("Edgar", "Anderson", role = "aut"),
  publisher = "American Iris Society",
  datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en",
  description = "The famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
)

test_that("dublincore works", {
  expect_true(is.dublincore(dct_iris))
})

test_that("dublincore() works", {
  expect_equal(dct_iris$language, 'en')
  expect_equal(dct_iris$publisher, "American Iris Society")
  expect_equal(dct_iris$year, "1935")
  expect_equal(dct_iris$datasource, "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
  expect_equal(dct_iris$identifier, ":tba")
  expect_equal(dct_iris$rights, ":tba")
  expect_equal(dct_iris$author, person("Edgar", "Anderson", role = "aut"))
  expect_equal(dct_iris$type, "DCMITYPE:Dataset")
  expect_equal(dct_iris$description, "The famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.")
})

x = iris_dataset

test_that("as_dublincore() works", {
  expect_true(is.dublincore(as_dublincore(iris_dataset)))
  expect_true(is.list(as_dublincore(x=iris_dataset, type="list")))
  expect_true(is.dataset(as_dublincore(iris_dataset, type="dataset")))
  expect_true(is.null(as_dublincore(iris_dataset, type="list")$subject))
  expect_true(is.null(as_dublincore(iris_dataset, type="list")$contributor))
  expect_equal(as_dublincore(iris_dataset, type="dataset")$description, "The famous (Fisher's or Anderson's) iris data set.")
  #expect_equal(dataset_title(as_dublincore(iris_dataset, type="dataset")), "The Dublin Core Metadata of `Iris Dataset'")
  expect_equal(as_dublincore(iris_dataset, type="dataset")$rights, ':unas')
  expect_equal(as_dublincore(iris_dataset, type="dataset")$coverage, ':unas')
})


test_that("as_dublincore() gives warning", {
  expect_warning(as_dublincore(iris_dataset, type = "character"))
})
