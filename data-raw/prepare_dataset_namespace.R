
library(tidyverse)
dataset_namespace <- readxl::read_excel(file.path("data-raw", "namespace.xlsx")) %>%
  mutate ( prefix = stringr::str_trim(prefix, "both"),
           uri  = stringr::str_trim(uri, "both")) %>%
  arrange(prefix)


usethis::use_data(dataset_namespace, overwrite = TRUE)
