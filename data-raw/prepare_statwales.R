library(tidyverse)
statwales <- readxl::read_excel(
  here::here("data-raw", "datacube_example.xlsx"),
  skip = 1
)

names(statwales) <- c(
  "refArea", "2004-2006-M", "2004-2006-F",
  "2005-2007-M", "2005-2007-F", "2006-2008-M", "2006-2008-F"
)


usethis::use_data(statwales, overwrite = TRUE)
