#' @title Available codelists by concept
#'
#' @description A dataset containing the available codelists in the StatCodelists package.
#' Currently it contains only SDMX harmonized concepts and their codelists.
#'
#' @format A data frame with 21 rows and 3 variables:
#' \describe{
#'   \item{concept}{A statistical concept, such as Age, Sex, Currency.}
#'   \item{codebook}{The name of the codebook, for example, CL_AGE.}
#'   \item{authority}{The standardizing authority, in this case, SDMX}
#'   ...
#' }
#' @source \url{https://sdmx.org/?page_id=3215/}
"codebooks"

