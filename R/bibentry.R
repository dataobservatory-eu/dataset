#' @title Create a bibentry for a dataset
#' @param ds A dataset.
#' @importFrom utils bibentry
#' @importFrom utils citation
#' @return A bibentry object for the \code{ds} dataset.
#' @param ds A data.frame or inherited tibble, data.frame, or a structured list.
#' @family citation functions
#' @examples
#' my_dataset <- dataset (
#'     x = data.frame (time = rep(c(2019:2022),2),
#'                     geo = c(rep("NL",4), rep("BE",4)),
#'                     value = c(1,3,2,4,2,3,1,5),
#'                     unit = rep("NR",8),
#'                     freq = rep("A",8)),
#'     Dimensions = c(1,2),
#'     Measures = 3,
#'     Attributes = c(4,5),
#'     sdmx_attributes = c("time", "freq"),
#'     Title = "Example dataset",
#'     Creator = person("Jane", "Doe"),
#'     Publisher = "Publishing Co.",
#'     Issued = as.Date("2022-07-14")
#' )
#'
#' bibentry(my_dataset)
#' utils::toBibtex(bibentry_dataset(my_dataset))
#' @export

bibentry_dataset <- function(ds) {

  edstractyear <- function(date) {
    format(date, format="%Y")
  }

  if ( is.numeric(attr(ds, "Issued"))) {
    year_nr <- attr(ds, "Issued")
  } else if (inherits(attr(ds, "Issued"), "POSIXt") | inherits(attr(ds, "issued"), "Date")) {
    year_nr <- edstractyear (attr(ds, "Issued"))
  } else {
    year_nr = attr(ds, "Issued")}

  entry_title <- ifelse(is.null(dataset_title(ds)$Subtitle),
                        dataset_title(ds)$Title,
                        paste0(dataset_title(ds)$Title, ". ", dataset_title(ds)$Subtitle))

  bibentry(
    bibtype = "Misc",
    title = entry_title,
    author = paste0(creator(ds)$family, ", ", creator(ds)$given),
    publisher = publisher(ds),
    size = attr(ds, "Size"),
    year = year_nr)
}
