#' A Small GDP Dataset
#'
#' A compact sample of GDP and main aggregates from Eurostat's annual
#' international cooperation dataset. This data subset contains illustrative
#' records for select countries and time periods.
#'
#' @format A data frame with 10 rows and 5 variables:
#' \describe{
#'   \item{geo}{Country name (character)}
#'   \item{year}{Reference year (integer)}
#'   \item{gdp}{Gross Domestic Product value (numeric)}
#'   \item{unit}{Unit of measurement, e.g., "Million EUR" (character)}
#'   \item{freq}{Observation frequency, e.g., "Annual" (character)}
#' }
#'
#' @details
#' This dataset is intended for examples, tests, and demonstration purposes. It
#' reflects simplified GDP data as published by Eurostat. The actual Eurostat
#' dataset includes more countries, breakdowns, and metadata.
#'
#' @source Eurostat (2021). GDP and main aggregates - international data
#' cooperation (annual data). \doi{10.2908/NAIDA_10_GDP}
#'
#' @keywords datasets
#' @examples
#' head(gdp)
"gdp"
