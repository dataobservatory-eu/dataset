df <- data.frame ( sex = c("M", "F"), value = c(1,2), unit = c("NR", "NR"))
dimensions(df, sdmx_attributes = "sex") <- "sex"
measures(df) <- "value"
attributes_measures(df) <- "unit"

test_that("dimensions() work", {

  expect_equal(dimensions(df)$names, c("sex"))
  expect_equal(dimensions(df)$isDefinedBy, c("https://purl.org/linked-data/cube|https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl"))
})

test_that("measures() work", {
  expect_true(inherits(measures(df), 'data.frame'))
  expect_equal(measures(df)$names, c("value"))
})

test_that("attributes_measures() work", {
  expect_true(inherits(attributes_measures(df), 'data.frame'))
  expect_equal(attributes_measures(df)$names, c("unit"))
})

x <- data.frame (
  time = rep(c(2019:2022),2),
  geo = c(rep("NL",4), rep("BE",4)),
  value = c(1,3,2,4,2,3,1,5),
  unit = rep("NR",8),
  freq = rep("A",8)
)


y <- dataset (x,
              Dimensions = c(1,2),
              Measures = 3,
              Attributes = c(4,5),
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
               rep("https://purl.org/linked-data/cube|https://raw.githubusercontent.com/UKGovLD/publishing-statistical-data/master/specs/src/main/vocab/sdmx-attribute.ttl",
                 2))
  expect_equal(names(y), c("time","geo", "value", "unit", "freq"))
})


test_that("bibentry_dataset", {
  expect_equal(bibentry_dataset(ds=y)$Title, "Example dataset")
  })

