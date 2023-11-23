#' @title Get/set the Publisher of the object.
#' @description Add the optional \code{Publisher} property as an attribute to an R object.
#' @details The \code{Publisher} corresponds to dct:publisher and Publisher in DataCite.
#' The name of the entity that holds, archives, publishes prints, distributes, releases,
#' issues, or produces the resource. This property will be used to formulate the citation,
#' so consider the prominence of the role. For software, use Publisher for the
#' code repository. If there is an entity other than a code repository, that
#' "holds, archives, publishes, prints, distributes, releases, issues, or produces" the
#' code, use the property Contributor/contributorType/ hostingInstitution for the code
#' repository.
#' @param x An R object, such as a data.frame, a tibble, or a data.table.
#' @param overwrite If the attributes should be overwritten. In case it is set to \code{FALSE},
#' it gives a message with the current \code{Publisher} property instead of overwriting it.
#' Defaults to \code{TRUE} when the attribute is set to \code{value} regardless of previous
#' setting.
#' @param value The \code{Publisher} as a character set.
#' @return The Publisher attribute as a character of length 1 is added to \code{x}.
#' @examples
#' iris_dataset <- iris
#' publisher(iris_dataset) <- "American Iris Society"
#' publisher(iris_dataset)
#' @family Reference metadata functions
#' @export
publisher<- function(x) {

  assertthat::assert_that(is.dataset(x),
                          msg = "publisher(x) must be a dataset object created with dataset() or as_dataset().")


  DataBibentry <- dataset_bibentry(x)
  DataBibentry$publisher
}

#' @rdname publisher
#' @export
`publisher<-` <- function(x,  overwrite = TRUE, value) {

  assertthat::assert_that(is.dataset(x),
                          msg = "publisher(x) must be a dataset object created with dataset() or as_dataset().")

  DataBibentry <- invisible(dataset_bibentry(x))

  if ( is.null(value)) {
    DataBibentry$publisher <- ":tba"
    attr(x, "DataBibentry") <- DataBibentry
    return(x)
  }

  if (length(value)>1) {
    stop("publisher(x) <- value: value must be of length 1.")
    }

  is_tba <- DataBibentry$publisher ==  ":tba"

  if (is.null(DataBibentry$publisher)) {
    DataBibentry$publisher <- value
    } else if (is_tba) {
      DataBibentry$publisher <- value
      }else if ( overwrite ) {
      DataBibentry$publisher <- value
  } else {
    message ("The dataset has already an Publisher: ",    DataBibentry$publisher )
  }

  attr(x, "DataBibentry") <- DataBibentry
  x
}
