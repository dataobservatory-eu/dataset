#' @title Create a globally unique row identifier
#' @description Add a globally unique row identifier to a dataset object.
#' @param ds A dataset object.
#' @param prefix The prefix of the globally unique wor identifier (URI or CURIe),
#' defaults to \code{"https:://example.org/my_data/"}.
#' @param keep_local_id Defaults to \code{FALSE}.
#' @family dataset functions
#' @return A dataset object with a locally unique row identifier added as a primary key to
#' the tabular form.
#' @examples
#' my_ds <- dataset (x = data.frame (
#'    time = rep(c(2019:2022),4),
#'    geo = c(rep("NL",8), rep("BE",8)),
#'    sex = c(rep("F", 4), rep("M", 4), rep("F", 4), rep("M", 4)),
#'    value = c(1,3,2,4,2,3,1,5, NA_real_, 4,3,2,1, NA_real_, 2,5),
#'    unit = rep("NR",8),
#'    freq = rep("A",8)),
#'  Dimensions = c("time", "geo", "sex"),
#'  Measures = "value",
#'  Attributes = c("unit", "freq"),
#'  sdmx_attributes = c("sex", "time", "freq"),
#'  Title = "Example dataset",
#'  Creator = person("Jane", "Doe"),
#'  Publisher = "Publishing Co.",
#'  Issued = as.Date("2022-07-14")
#')
#' dataset_uri(my_ds)
#' @export

dataset_uri <- function(ds,
                        prefix = "https:://example.org/my_data/",
                        keep_local_id = FALSE) {

  if ( is.null(attr(ds, "local_id")) ) {
    ds <- dataset_local_id(ds)
  } else if ( attr(ds, "local_id") %in% names(ds) ) {
    ds <- dataset_local_id(ds)
  }

  ds$URI <- paste0(prefix, ds$local_id)

  return_ds <- cbind(
    subset(ds, select = "URI"),
    subset(ds, select = names(ds)[names(ds)!= "URI"])
  )

  if ( keep_local_id == FALSE ) {

    attr(return_ds, "local_id") <- NULL
    return_ds$local_id <- NULL
  }

  attr(return_ds, "URI") <- "URI"
  return_ds
}
