#' @title Create a locally unique id
#' @description Add a locally unique row identifier to a dataset object.
#' @param ds A dataset object.
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
#' @export

dataset_local_id <- function(ds) {

  time_var <- time_var_guess(ds)
  all_unique <- function(x) length(unique(x))==1
  which_cols_constant <- vapply(names(ds), function(x) all_unique(ds[[x]]), logical(1))
  which_cols_constant <-  names(which_cols_constant[which_cols_constant])


  not_measures <- base::union(attributes_measures(ds)$names, dimensions(ds)$names)
  reduce_columns <-  not_measures[! not_measures %in% c(time_var, which_cols_constant, measures(ds)$names)]

  ds$local_id <- vector(mode="character", length = nrow(ds))

  for ( i in seq_along(reduce_columns) ) {
    char_value <- as.character(ds [[reduce_columns[i]]])
    char_value <- paste0(reduce_columns[i], "=", char_value)
    ds$local_id <- paste0(ds$local_id, char_value, "_")
  }

  if ( !is.null(time_var) ) {
    ds$local_id <- paste0(ds$local_id, time_var, "=", as.character(ds[[time_var]]))
  } else {
    ds$local_id <- substr(ds$local_id, start = 1, stop = nchar(ds$local_id)-1)
  }

  return_ds <- cbind(
    subset(ds, select = "local_id"),
    subset(ds, select = names(ds)[names(ds) != "local_id"])
           )

  attr(return_ds, "local_id") <- "local_id"

  row.names(return_ds) <- NULL
  return_ds
}

#' @keywords internal
time_var_guess <- function(df) {

  time_var <- NULL
  time_var <- names(df)[vapply ( seq_len(ncol(df)), function(x) inherits(df[[x]], "Date"), logical(1))]

  if ( is.null(time_var) | length(time_var) == 0 )  {
    possible_time_var <- which (tolower(names(df)) %in% c("time", "year", "date"))
    if (length(possible_time_var)==1) {
      names(df)[possible_time_var]
    } else {
      return(NULL)
    }
  } else {
    time_var
  }

}
