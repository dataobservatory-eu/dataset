try_dataset_download <- function(){
  dest_file <- file.path(tempdir(), "5813772.csv")
  try(dataset_download(
    url = "https://dataset.dataobservatory.eu/environmental_transfer_from_europe_test_dataset.csv",
    Title = "Environmental Subsidies and Similar Transfers from Europe to the Rest of the World",
    Dimensions = c("time", "geo"),
    Measures = "value",
    Attributes = c("unit", "obs_status", "method", "freq"),
    Identifier = "https://doi.org/10.5281/zenodo.5813772",
    destfile = dest_file
  ))
}

test_download <- try_dataset_download()

test_that("dataset_download() works", {
  testthat::skip_if_offline()
  expect_equal(names(test_download),
               c('dataset_code', 'time', 'geo', 'value', 'unit', 'obs_status',
                 'method', 'freq'))
  expect_true(grepl('KiB', attr(test_download, "Size")))
  expect_equal(identifier(test_download), identifier(test_download))
  expect_equal(dataset_source(test_download), "https://dataset.dataobservatory.eu/environmental_transfer_from_europe_test_dataset.csv")
 })




