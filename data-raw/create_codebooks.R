## code to prepare `DATASET` dataset goes here
library(dplyr)
library(tibble)
sdmx_codebooks <- tibble::tribble(
  ~concept, ~codebook,
  "Activity", "CL_ACTIVITY",
  "Age", "CL_AGE",
  "Civil or marital status", "CL_CIVIL_STATUS",
  "Classification of Individual Consumption According to Purpose (COICOP)", "CL_COICOP",
  "Classification of the Functions of Government (COFOG)", "CL_COFOG",
  "Classification of the Outlays of Producers According to Purpose (COPP)", "CL_COPP",
  "Classification of the Purposes of Non-Profit Institutions Serving Households COPNI", "CL_COPNI",
  "Confidentiality status", "CL_CONF_STATUS",
  "Currency", "CL_CURRENCY",
  "Decimals", "CL_DECIMALS",
  "Degree of Urbanisation", "CL_DEG_URB",
  "Frequency", "CL_FREQ",
  "Geographical areas", "CL_AREA",
  "Observation status", "CL_OBS_STATUS",
  "Occupation", "CL_OCCUPATION",
  "Organisation concepts", "CL_ORGANISATION",
  "Seasonal adjustment", "CL_SEASONAL_ADJUST",
  "Sex", "CL_SEX",
  "Time format", "CL_TIME_FORMAT",
  "Time period – collection", "CL_TIME_PER_COLLECT",
  "Unit multiplier", "CL_UNIT_MULT"
)

sdmx_codebooks$authority <- "SDMX"
codebooks <- as.data.frame(sdmx_codebooks)


usethis::use_data(codebooks, overwrite = TRUE)
