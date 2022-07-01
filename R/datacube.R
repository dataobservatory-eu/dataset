
arg.names <- function (args) {
  if (deparse (args) == "c()") { return (character(0)) }
  if (deparse (args) == "list()") { return (character(0)) }
  if (is.null (args)) { return (NULL) }
  names <- sapply (args, deparse)
  if (length (names) > 1) { names <- names [-1] }
  return (names)
}


dot.names <- function (dots) {
  if (length (dots) > 0 && is.null (get_expr (dots [[1]]))) { return (NULL) }
  names <- sapply (dots, rlang::quo_name)
  if (length (names) == 0) { return (character(0)) }
  return (names)
}



#' @title Structure a data frame to a datacube
#' @description Coerce a data.frame or a tbl_df object into a datacube.
#' @details Loosely follows the \href{https://www.w3.org/TR/vocab-data-cube/}{The RDF Data Cube Vocabulary}.
#' See \href{https://www.w3.org/TR/vocab-data-cube/#dsd-dimensions}{Dimensions, attributes and measures}
#' of the Data Cube.
#' @param x A data.frame.
#' @param tbl_df A tbl_df (tibble) object.
#' @param obs_id The observation identifier. Defaults to \code{NULL} in which case
#' the combination of the dimensions is used. If there are no dimensions present, then the
#' \code{row.names} will be used from \code{x}.
#' @param dim_names The columns in the data frame that are dimensions, and maybe used
#' for statistical aggregation.
#' @param measure_names The measurements in the data frame.
#' @param attribute_names Not measured attributes of the observations in the data.frame.
#' @param unit The unit of the measurements
#' @importFrom assertthat assert_that
#' @family datacube functions
#' @return A datacube object, which has at least observation ids.
#' @examples
#' as.datacube (x = iris[1:6,],
#'              obs_id = NULL,
#'              dim_names = NULL,
#'              measure_names = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'              attribute_names = "Species"
#'              )
#' @export
as.datacube <- function (x, ...) { UseMethod ("as.datacube") }

#' @rdname as.datacube
#' @export
is.datacube  <- function(x) inherits(x, "datacube")

#' @export
as.datacube.data.frame <- function (x,
                                    obs_id = NULL,
                                    dim_names  = NULL,
                                    measure_names = NULL,
                                    attribute_names = NULL) {

  dim.names <- names(x)[which(dim_names %in% names(x))]
  dim_names_not_present <- paste(names(x)[! which(dim_names %in% names(x))], collapse = ", ")
  measure_names_not_present <- paste(names(x)[! which(measure_names %in% names(x))], collapse = ", ")
  attribute_names_not_present <- paste(names(x)[! which(attribute_names %in% names(x))], collapse = ", ")

  assert_that(nchar(dim_names_not_present)==0,
              msg = glue::glue("The following dim_names are not present in the x: {dim_names_not_present}"))

  assert_that(nchar(measure_names_not_present)==0,
              msg = glue::glue("The following measure_names are not present in the x: {measure_names_not_present}"))


  assert_that(nchar(attribute_names_not_present)==0,
              msg = glue::glue("The following attribute_names are not present in the x: {attribute_names_not_present}"))


  dc_dims <- subset(x, select = dim_names)
  dc_measures <- subset(x, select = measure_names )
  dc_attributes <- subset (x, select = attribute_names )

  if (is.null(obs_id)) {
    if ( length(dim.names)>0 ) {
      dc <- as.data.frame(tidyr::unite(dc_dims, col = "obs_id"))
    }  else {
      dc <- data.frame(
        obs_id = row.names(x)
      )
    }
  } else {
    dc <- subset(x, select = obs_id)
  }

  return_x <- cbind(dc, dc_dims, dc_measures, dc_attributes)
  attr(return_x, "class")  <- c("datacube", class(return_x))
  attr(return_x, "observations") <- names(return_x)[1]
  attr(return_x, "dimensions") <- names(dc_dims)
  attr(return_x, "measures") <- names(dc_measures)
  attr(return_x, "attributes") <- names(dc_attributes)
  return_x

}

#' @export
as.datacube.tibble  <- function (x,
                                 obs_id = NULL,
                                 dim_names  = NULL,
                                 measure_names = NULL,
                                 attribute_names = NULL) {


  dim.names <- names(x)[which(dim_names %in% names(x))]
  dim_names_not_present <- paste(names(x)[! which(dim_names %in% names(x))], collapse = ", ")
  measure_names_not_present <- paste(names(x)[! which(measure_names %in% names(x))], collapse = ", ")
  attribute_names_not_present <- paste(names(x)[! which(attribute_names %in% names(x))], collapse = ", ")

  assert_that(nchar(dim_names_not_present)==0,
              msg = glue::glue("The following dim_names are not present in the x: {dim_names_not_present}"))

  assert_that(nchar(measure_names_not_present)==0,
              msg = glue::glue("The following measure_names are not present in the x: {measure_names_not_present}"))


  assert_that(nchar(attribute_names_not_present)==0,
              msg = glue::glue("The following attribute_names are not present in the x: {attribute_names_not_present}"))


  dc_dims <- subset(x, select = dim_names)
  dc_measures <- subset(x, select = measure_names )
  dc_attributes <- subset (x, select = attribute_names )

  if (is.null(obs_id)) {
    if ( length(dim.names)>0 ) {
      dc <- tidyr::unite(dc_dims, col = "obs_id")
    }  else {
      dc <- tibble::tibble(
        obs_id = row.names(x)
      )
    }
  }


  class (dc_attributes)
  return_datacube <- bind_cols(dc, dc_dims, dc_measures, dc_attributes)
  attr(return_datacube, "class")  <- c("datacube", class(return_x))
  attr(return_datacube, "observations") <- names(return_x)[1]
  attr(return_datacube, "dimensions") <- names(dc_dims)
  attr(return_datacube, "measures") <- names(dc_measures)
  attr(return_datacube, "attributes") <- names(dc_attributes)
  return_datacube
}

#' @rdname as.datacube
print.datacube <- function (x, ...) {

  NextMethod()
 # UseMethod ("print.datacube")\
  cat(paste0('observations: "', paste(attr(x, "observations"), collapse = '" "'), '"\n'))
  cat(paste0('dimensions: "', paste(attr(x, "dimensions"), collapse = '" "'), '"\n'))
  cat(paste0('measures: "', paste(attr(x, "measures"), collapse = '" "'), '"\n'))
  cat(paste0('attributes: "', paste(attr(x, "attributes"), collapse = '" "'), '"\n'))

}

#' @rdname as.datacube
summary.datacube <- function (x, ...) { UseMethod ("summary.datacube") }
summary.datacube.data.frame <- function(x, ...) { NextMethod()}
summary.datacube.tbl_df <- function(x, ...) { NextMethod()}


