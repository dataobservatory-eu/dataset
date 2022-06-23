#' @title Available codelists by concept
#'
#' @description A dataset containing the available codelists in the StatCodelists package.
#' @details The SDMX description: "This code list provides a set of values indicating the
#'\code{frequency} of the data (e.g. weekly, monthly, quarterly). The concept\code{frequency}
#' may refer to various stages in the production process, e.g. data collection or data
#' dissemination. For example, a time series could be disseminated at annual frequency but
#' the underlying data are compiled monthly. The code list is applicable for all different
#' uses of\code{frequency"}."
#' @format A data frame with 34 rows and 5 variables:
#' \describe{
#'   \item{id}{A statistical code for describing data collection frequency.}
#'   \item{name}{The standard label of the code.}
#'   \item{description}{The description of the code.}
#'   \item{name_local}{The language code of the name label.}
#'   \item{description_locale}{The language code of the description.}
#'   ...
#' }
#' @source \url{https://sdmx.org/?page_id=3215/}
"CL_FREQ"
