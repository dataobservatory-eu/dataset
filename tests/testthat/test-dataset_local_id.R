my_ds <- dataset (x = data.frame (
  time = rep(c(2019:2022),4),
  geo = c(rep("NL",8), rep("BE",8)),
  sex = c(rep("F", 4), rep("M", 4), rep("F", 4), rep("M", 4)),
  value = c(1,3,2,4,2,3,1,5, NA_real_, 4,3,2,1, NA_real_, 2,5),
  unit = rep("NR",8),
  freq = rep("A",8)),
  Dimensions = c("time", "geo", "sex"),
  Measures = "value",
  Attributes = c("unit", "freq"),
  sdmx_attributes = c("sex", "time", "freq"),
  Title = "Example dataset",
  Creator = person("Jane", "Doe"),
  Publisher = "Publishing Co.",
  Issued = as.Date("2022-07-14")
)

test_value <- paste0("geo=", my_ds$geo, "_sex=", my_ds$sex, "_time=", my_ds$time)

test_that("dataset_local_id() works", {
  expect_equal(dataset_local_id(ds =  my_ds)$local_id, test_value )
})
