#' @title Get/set the Bibentry of the object.
#' @description The \code{\link{dataset_df}} objects contain among their
#' attributes bibliographic entries which are stored in a
#' \code{\link[utils:bibentry]{utils::bibentry}} object. Upon creation, these
#' entries are filled with default values when applicable. \cr \cr To retrieve
#' the bibentry of a dataset_df object, use \code{get_bibentry}.\cr \cr To
#' create a new bibentry, use the \code{\link{datacite}} function for an
#' interface and default values according to the DataCite standard, or the
#' \code{\link{dublincore}} function for the more general Dublin Core
#' standard.\cr \cr To change or an entire new bibliographic entry to a
#' dataset_df object (or any data.frame-like object), use the
#' \code{`set_bibentry<-`} function (see examples.) For more details, please
#' check the \code{vignette("bibentry", package="dataset")} vignette.
#' @param dataset A dataset created with \code{\link{dataset_df}}.
#' @param value A \code{\link[utils:bibentry]{utils::bibentry}} object, or a
#'   newly initialised bibentry object with DataCite default values for
#'   unassigned entries.
#' @importFrom utils bibentry
#' @importFrom utils person
#' @return The \code{get_bibentry} returns from the
#'   \code{\link[utils]{bibentry}} object of \code{x} from its attributes; the
#'   \code{`set_bibentry<-`} assignment function sets this attribute to
#'   \code{value} and invisibly returns \code{x} with the changed attributes. To
#'   set well-formatted input \code{value}, refer to \code{\link{datacite}} or
#'   \code{\link{dublincore}} (see Details.)
#' @family bibentry functions
#' @examples
#' # Get the bibentry of a dataset_df object:
#' iris_bibentry <- get_bibentry(iris_dataset)
#'
#' # Create a well-formatted bibentry object:
#' alternative_bibentry <- datacite(
#'   Creator = person("Jane Doe"),
#'   Title = "The Famous Iris Dataset",
#'   Publisher = "MyOrg"
#' )
#'
#' # Assign the new bibentry object:
#' set_bibentry(iris_dataset) <- alternative_bibentry
#'
#' # Print the bibentry object according to the DataCite notation:
#' as_datacite(iris_dataset, "list")
#'
#' # Print the bibentry object according to the Dublin Core notation:
#' as_dublincore(iris_dataset, "list")
#' @export

get_bibentry <- function(dataset) {
  assertthat::assert_that("dataset_bibentry" %in% names(attributes(dataset)),
    msg = "Error: get_bibentry(dataset): dataset has no dataset_bibentry attribute"
  )

  attr(dataset, "dataset_bibentry")
}

#' @rdname get_bibentry
#' @export
`set_bibentry<-` <- function(dataset, value) {
  sys_time <- Sys.time()
  year <- substr(as.character(sys_time), 1, 4)

  if (is.null(value)) {
    value <- dublincore(
      title = "Untitled Dataset",
      creator = person("Unknown Author"),
      dataset_date = year
    )
  }

  if (is.null(value$year)) value$year <- year

  attr(dataset, "dataset_bibentry") <- value
  invisible(dataset)
}

#' @keywords internal
set_default_bibentry <- function() {
  sys_time <- Sys.time()
  year <- substr(as.character(sys_time), 1, 4)
  Title <- "Untitled Dataset"
  Creator <- person("Unknown", "Author")
  dataset_bibentry <- datacite(Title = Title, Creator = Creator, PublicationYear = year)
  dataset_bibentry
}
