test_that("fix_publisher() works", {
  expect_equal(fix_publisher(publishers = "publisher"), "publisher")
  expect_equal(fix_publisher(publishers = NULL), ":unas")
  expect_equal(fix_publisher(publishers = person("Jane", "Doe", role = "pbl")), "Jane")
  expect_equal(fix_publisher(publishers = person("American Iris Society", role = "pbl")), "American Iris Society")
  expect_equal(fix_publisher(publishers = c(
    person("Jane", "Doe", role = "pbl"),
    person("American Iris Society")
  )), "{Jane} and {American Iris Society}")
})

test_that("fix_contributors() works", {
  expect_equal(fix_contributor(contributors = "contributor"), "contributor")
  expect_equal(fix_contributor(contributors = NULL), ":unas")
  expect_equal(fix_contributor(contributors = person("Jane", "Doe", role = "ctb")), "Jane Doe [ctb]")
  expect_equal(fix_contributor(contributors = person("American Iris Society", role = "pbl")), "American Iris Society [pbl]")
  expect_equal(
    fix_contributor(contributors = c(
      person("Jane", "Doe", role = "ctb"),
      person("Joe", "Doe", comment = c(ORCID = "1234"))
    )),
    "{Jane Doe [ctb]} and {Joe Doe (1234)}"
  )
})



test_that("new_dublincore() works", {
  expect_equal(
    new_dublincore(
      title = "Test",
      creator = person("Jane", "Doe", role = "cre")
    )$author,
    person("Jane", "Doe", role = "cre")
  )
  expect_equal(new_dublincore(
    title = "Test",
    creator = c(person("Jane", "Doe", role = "cre"), person("Joe", "Doe", role = "cre"))
  )$author, c(person("Jane", "Doe", role = "cre"), person("Joe", "Doe", role = "cre")))
  expect_equal(
    new_dublincore(title = "Test", creator = person("Jane", "Doe", role = "cre"))$title,
    "Test"
  )
  expect_equal(
    new_dublincore(
      title = "Test",
      creator = person("Jane", "Doe", role = "cre"),
      contributor = c(
        person("Joe", "Doe", role = "dtm"),
        person("Daniel", "Antal", role = "ctb")
      )
    )$contributor,
    "{Joe Doe [dtm]} and {Daniel Antal [ctb]}"
  )
  expect_equal(
    new_dublincore(
      title = "Test",
      creator = person("Jane", "Doe", role = "cre"),
      publisher = person("My Publisher Inc.", role = "pbl")
    )$publisher,
    "My Publisher Inc."
  )
  expect_equal(
    new_dublincore(
      title = "Test",
      creator = person("Jane", "Doe", role = "cre"),
      datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
    )$datasource,
    "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
  )
  expect_equal(
    new_dublincore(
      title = "Test",
      creator = person("Jane", "Doe", role = "cre"),
      dataset_date = 1935
    )$date,
    "1935"
  )
  expect_equal(
    new_dublincore(
      title = "Test",
      creator = person("Jane", "Doe", role = "cre"),
      language = "en"
    )$language,
    "en"
  )
})


test_that("dublincore works", {
  dct_iris1 <- dublincore(
    title = "Iris Dataset",
    creator = c(
      person(given = "Edgar", family = "Anderson", role = "aut"),
      person(given = "Jane D", family = "Anderson", role = "cre")
    ),
    publisher = person("American Iris Society", role = "pbl"),
    datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
    dataset_date = 1935,
    language = "en",
    description = "The famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
  )
  expect_equal(dct_iris1$author, c(
    person(given = "Edgar", family = "Anderson", role = "aut"),
    person(given = "Jane D", family = "Anderson", role = "cre")
  ))
  expect_equal(dct_iris1$contributor, ":unas")
  expect_equal(dct_iris1$publisher, "American Iris Society")
  dct_iris <- dublincore(
    title = "Iris Dataset",
    creator = person("Edgar", "Anderson", role = "aut"),
    publisher = person("American Iris Society", role = "pbl"),
    contributor = person("Daniel", "Antal", role = "dtm"),
    datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
    dataset_date = 1935,
    language = "en",
    description = "The famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
  )
  expect_equal(dct_iris$publisher, "American Iris Society")
  expect_equal(dct_iris$contributor, "Daniel Antal [dtm]")
  expect_equal(dct_iris$date, "1935")
  expect_true(is.dublincore(dct_iris))
})


test_that("dublincore() works", {
  dct_iris <- dublincore(
    title = "Iris Dataset",
    creator = person("Edgar", "Anderson", role = "aut"),
    publisher = "American Iris Society",
    datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
    dataset_date = 1935,
    language = "en",
    description = "The famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
  )
  expect_equal(dct_iris$language, "en")
  expect_equal(dct_iris$publisher, "American Iris Society")
  expect_equal(dct_iris$date, "1935")
  expect_equal(dct_iris$datasource, "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
  expect_equal(dct_iris$identifier, ":tba")
  expect_equal(dct_iris$rights, ":tba")
  expect_equal(dct_iris$author, person("Edgar", "Anderson", role = "aut"))
  expect_equal(dct_iris$type, "DCMITYPE:Dataset")
  expect_equal(dct_iris$description, "The famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.")
})

test_that("as_dublincore() works", {
  expect_true(is.dublincore(as_dublincore(x = iris_dataset)))
  expect_true(is.list(as_dublincore(x = iris_dataset, type = "list")))
  expect_equal(
    as_dublincore(iris_dataset, type = "list")$contributor,
    "Antal Daniel [dtm]"
  )
  expect_equal(as_dublincore(iris_dataset)$date, "1935")
  expect_equal(as_dublincore(iris_dataset)$description, "The famous (Fisher's or Anderson's) iris data set.")
  expect_equal(as_dublincore(iris_dataset)$rights, ":tba")
  expect_equal(as_dublincore(iris_dataset)$coverage, ":unas")
  iris_dc_triples <- as_dublincore(iris_dataset, "ntriples")
  expect_equal(iris_dc_triples[1], '<https://doi.org/10.5281/zenodo.10396807> <http://purl.org/dc/terms/title> \"Iris Dataset\"^^<http://www.w3.org/2001/XMLSchema#string> .')
})


test_that("as_dublincore() gives warning", {
  expect_warning(as_dublincore(iris_dataset, type = "character"))
})

test_that("dublincore() new example works", {
  orange_bibentry <- dublincore(
    title = "Growth of Orange Trees",
    creator = c(
      person(
        given = "N.R.",
        family = "Draper",
        role = "cre",
        comment = c(VIAF = "http://viaf.org/viaf/84585260")
      ),
      person(
        given = "H",
        family = "Smith",
        role = "cre"
      )
    ),
    contributor = person(
      given = "Antal",
      family = "Daniel",
      role = "dtm"
    ), # Add data manager
    publisher = "Wiley",
    datasource = "https://isbnsearch.org/isbn/9780471170822",
    dataset_date = 1998,
    identifier = "https://doi.org/10.5281/zenodo.14917851",
    language = "en",
    description = "The Orange data frame has 35 rows and 3 columns of records of the growth of orange trees."
  )
  expect_equal(orange_bibentry$description, "The Orange data frame has 35 rows and 3 columns of records of the growth of orange trees.")
})

as_dublincore(orange_df)
