small_country_dataset <- dataset_df(
  country_name = defined(c("Andorra", "Lichtenstein"), label  = "Country"),
  gdp = defined(c(3897, 7365),
                      label = "Gross Domestic Product",
                      unit = "million dollars")
)

var_definition(small_country_dataset$country_name) <- "http://data.europa.eu/bna/c_6c2bb82d"


test_that("var_definition() works", {
  expect_equal(var_definition(small_country_dataset$country_name), "http://data.europa.eu/bna/c_6c2bb82d")
})


