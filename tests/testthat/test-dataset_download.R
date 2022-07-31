
dest_file <- file.path(tempdir(), "5813772.csv")
dataset_download(
    Title = "Environmental Subsidies and Similar Transfers from Europe to the Rest of the World",
    dimensions = c("time", "geo"),
    measures = "value",
    attributes = c("unit", "obs_status", "method", "freq"),
    identifer = "https://doi.org/10.5281/zenodo.5813772",
    destfile = dest_file,
    url = "https://doi.org/10.5281/zenodo.5813772"
)


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
