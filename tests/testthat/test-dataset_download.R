try_dataset_download <- function(){
  dest_file <- file.path(tempdir(), "iris.csv")
  try(dataset_download(
    url = "https://zenodo.org/record/7421899/files/iris.csv?download=1",
    title = "Iris Dataset",
    author = person(given="Edgar", family="Anderson"),
    publisher = "American Iris Society",
    identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
    destfile = dest_file
  ))
}

test_download <- try_dataset_download()

test_that("dataset_download() works", {
  testthat::skip_if_offline()
  expect_equal(names(test_download),
               c("Sepal.Length", "Sepal.Width" ,"Petal.Length","Petal.Width" ,"Species"))
  expect_equal(dataset_title(test_download), "Iris Dataset")
})
