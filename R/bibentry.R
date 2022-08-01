#' @inheritParams dataset
#' @importFrom utils bibentry
#' @importFrom utils citation
#' @rdname dataset
#' @family citation functions
#' @export
bibentry_dataset <- function(x) {

  extractyear <- function(date) {
    format(date, format="%Y")
  }

  if ( is.numeric(attr(x, "Issued"))) {
    year_nr <- attr(x, "Issued")
  } else if (inherits(attr(x, "Issued"), "POSIXt") | inherits(attr(x, "issued"), "Date")) {
    year_nr <- extractyear (attr(x, "Issued"))
  } else {
    year_nr = attr(x, "Issued")}

  paste0()

  entry_title <- ifelse(is.null(dataset_title(x)$Subtitle),
                        dataset_title(x)$Title,
                        paste0(dataset_title(x)$Title, ". ", dataset_title(x)$Subtitle))

  bibentry(
    bibtype = "Misc",
    title = entry_title,
    author = paste0(creator(x)$family, ", ", creator(x)$given),
    publisher = publisher(x),
    size = attr(x, "Size"),
    year = year_nr)
}
