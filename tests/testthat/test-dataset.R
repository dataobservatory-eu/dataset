x <- data.frame (
  time = rep(c(2019:2022),2),
  geo = c(rep("NL",4), rep("BE",4)),
  value = c(1,3,2,4,2,3,1,5),
  unit = rep("NR",8),
  freq = rep("A",8)
)

y <- dataset (x,
              dimensions = c(1,2),
              measures = 3,
              attributes = c(4,5),
              sdmx_attributes = c("time", "freq"),
              Title = "Example dataset",
              Creator = person("Jane", "Doe"),
              Publisher = "Publishing Co.",
              Issued = as.Date("2022-07-14")
              )

df <- data.frame( sex = c("M", "F"), value = c(1,2))

test_that("dataset", {
  expect_equal(attributes(y)$dimensions$names, c("time", "geo"))
  expect_equal(attributes(y)$measures$names, c("value"))
  expect_equal(attributes(y)$attributes$names, c("unit", "freq"))
  expect_equal(as.character(attributes(y)$dimensions$isDefinedBy),
               c("https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl",
                 "http://purl.org/linked-data/cube"))
  expect_equal(attributes(dimensions_add(x =df, dimensions = "sex"))$dimensions$names, "sex")
  expect_equal(attributes(dimensions_add(x =df, dimensions = "sex", sdmx_attributes = "sex"))$dimensions$isDefinedBy, c(sex = "https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl")
)
})

test_that("bibentry_dataset", {
  expect_equal(bibentry_dataset(y)$Title, "Example dataset")
  })





#iris_ds <- as.datacube ( x = iris[1:6,],
#                         obs_id = NULL,
#                         dim_names = NULL,
#                         measure_names = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#                         attribute_names = "Species")

#iris_dataset <- dataset(iris_ds)
#test_that("dataset", {
#  expect_equal(attr(iris_dataset, "Title"), "Untitled Dataset")
#  expect_equal(attr(iris_dataset, "Creator")$given, "unknown creator")
#  expect_equal(attr(iris_dataset, "Publisher"), "<not yet published>")
#  expect_equal(attr(iris_dataset, "PublicationYear"), as.integer(substr(as.character(Sys.Date()),1,4)
#))
#})

