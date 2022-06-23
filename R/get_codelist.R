#' @title Get a codelist
#' @return A codelist in a data frame.
#' @param codelist The abbreviation of the codelist.
#' @param var_name The name of the \code{id} identifier variable that is coded.
#' @export

get_codelist <- function( codelist = "CL_FREQ", var_name = "id") {

  data("CL_FREQ", envir = environment())

  if (var_name != "id") {
    names(CL_FREQ)[1] <- var_name
    CL_FREQ
  } else {
    CL_FREQ
  }
}
