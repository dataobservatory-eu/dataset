#' @title Get or update provenance information
#' @description Add or update information about the history (provenance) of the dataset.
#' @details
#' For additional details see \code{vignette("provenance", package = "dataset")}.
#' @inheritParams dataset
#' @param value A list that may contain the following elements: \code{wasInformedBy},
#' \code{wasAssociatedWith}.
#' @examples
#' # Get the provenance of a dataset:
#' provenance(iris_dataset)
#'
#' # Update the provenance:
#' provenance(iris_dataset) <- list(
#'           wasInformedBy="https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
#'           )
#' @export
provenance <- function(x) {
  if(!is.dataset(x)) {
    stop("provenance(x): x must be a dataset object with standardised provenance metadata.")
  }
  attr(x, "Provenance")
}

#' @rdname provenance
#' @export
`provenance<-` <- function(x,  value) {

  if (!is.dataset(x)) {
    stop("provenance(x): x must be a dataset object created with dataset() or as_dataset().")
  }

  old_provenance <- provenance(x)
  new_provenance <- old_provenance


  if ("wasAssocitatedWith" %in% names(value)) {
    ## Add association attributes
    associated_with <- c(old_provenance$wasAssocitatedWith, value$wasAssocitatedWith)
    associated_with <- unique(associated_with)
    new_provenance$wasAssocitatedWith <- associated_with
  }

  if ("wasInformedBy" %in% names(value)) {
    ## Add wasInformedBy attributes
    if("wasInformedBy" %in% names(old_provenance)) {
      was_informed_by <- c(old_provenance$wasInformedBy, value$wasInformedBy)
    } else {
      was_informed_by <- value$wasInformedBy
    }
    new_provenance$wasInformedBy <- was_informed_by
  }

  attr(x, "Provenance") <- new_provenance
  x
}
