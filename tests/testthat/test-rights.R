myiris <- dataset(
  x = iris,
  title = "Iris Dataset",
  author = person("Edgar", "Anderson", role = "aut"),
  source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  date = 1935,
  language = "en",
  description = "This famous (Fisher's or Anderson's) iris data set."
)


test_that("rights() works", {
  expect_equal(rights(myiris), ":unas")
  expect_error(rights(myiris) <- c(":unas", "hello"))
})

iris_dataset2 <- myiris

rights(myiris)
rights(iris_dataset2, overwrite=TRUE) <- "CC-BY-SA"
rights(iris_dataset2)

test_that("rights() works", {
  expect_equal(rights(myiris), ":unas")
  expect_equal(rights(iris_dataset2),  "CC-BY-SA")
  expect_equal(class(rights(iris_dataset2)), "character")
  expect_equal(rights(myiris, FALSE) <- "CC0", "CC0")  #change :unas is not an overwrite
  expect_message(rights(myiris, FALSE) <- "CC1")
})

