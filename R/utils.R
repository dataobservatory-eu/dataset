
#' @keywords internal
#' @importFrom utils data
getdata <- function(...)
{
  e <- new.env()
  name <- utils::data(..., envir = e)[1]
  e[[name]]
}
