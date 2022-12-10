try_dataset_download <- function(){
  dest_file <- file.path(tempdir(), "iris.csv")
  try(dataset_download(
    url = "https://zenodo.org/record/7421899/files/iris.csv?download=1",
    Dimensions = NULL,
    Measures = c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ),
    Attributes = "Species",
    Title = "Iris Dataset",
    Publisher = "American Iris Society",
    Identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
    destfile = dest_file
  ))
}

test_download <- try_dataset_download()

test_that("dataset_download() works", {
  testthat::skip_if_offline()
  expect_equal(names(test_download),
               c("Sepal.Length", "Sepal.Width" ,"Petal.Length","Petal.Width" ,"Species"))
  expect_true(grepl('KiB', attr(test_download, "Size")))
  expect_equal(identifier(test_download), identifier(test_download))
  expect_equal(dataset_source(test_download), "https://zenodo.org/record/7421899/files/iris.csv?download=1")
 })
