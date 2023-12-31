#' @title Create a dataset
#'
#' @param x An R object that contains the data of the dataset (a data.frame or
#' inherited from [`data.frame`][base::data.frame()]), for example,
#' [tibble::tibble()], [tsibble::tsibble()], [data.table::data.table()].
#' @param author A single person or a vector of persons as authors, declared with
#' \code{\link[utils:person]{person}}.
#' @param title The title of the dataset.
#' @param identifier The permanent identifier, for example, the DOI of the dataset.
#' If left empty (NULL), receives the DataCite standard value \code{':tba'}, or to
#' be announced later.
#' @param publisher The organisation or person that publishes the dataset.
#' If left empty (NULL), receives the DataCite standard value \code{':tba'}, or to
#' be announced later.
#' @param year The year of the creation of the dataset. If left empty (NULL),
#' the current year.
#' @param version The version of the dataset. If left empty (NULL), defaults to
#' \code{'0.1.0'}
#' @param datasubject The subject of the data frame, as a [subject] type.
#' @param description The optional \code{Description} property as an attribute to
#' an R object.
#' @param language The primary language of the dataset, for example \code{'eng'}.
#' Defaults to \code{NULL} that sets it to the unassigned value \code{':unas'}.
#' @param datasource The source of the dataset,
#' \href{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/source/}{DCMI: Source},
#' which corresponds to a \code{relatedItem} in the DataCite vocabulary. We use
#' \code{datasource} instead of \code{source} to avoid naming conflicts with the
#' base R  \code{source()} function.
#' @param rights Any rights information for this resource. The property may be
#' repeated to record complex rights characteristics.
#' Free text, defaults to \code{":unas"} for unassigned values. See \code{\link{rights}}.
#' @return A dataset object, which is a data.frame or inherited object with rich
#' metadata.
#' @seealso [xsd_convert()]
#' @examples
#' ds <- dataset(iris,
#'         title = "The iris Dataset",
#'         author = c(
#'            person(family ="Anderson",
#'            given ="Edgar",
#'            role = "aut")
#'            ),
#'          identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
#'          year = "1935",
#'          version = "1.0",
#'          description = "The famous dataset that is distributed with R.",
#'          url = "https://en.wikipedia.org/wiki/Iris_flower_data_set",
#'          resourceType = "Dataset"
#'          )
#' @export
dataset  <- function(x,
                     author,
                     title,
                     identifier = NULL,
                     publisher = NULL,
                     year = NULL,
                     version = NULL,
                     datasubject = NULL,
                     description = NULL,
                     language = NULL,
                     datasource = NULL,
                     rights = NULL,
                     ... ) {

  arguments <- list(...)

  if (is.null(datasubject)) {
    datasubject <- subject_create(NULL)
  }

  as_dataset(x,
             author=author,
             title=title,
             publisher=publisher,
             identifier=identifier,
             year=year,
             version=version,
             datasubject=datasubject,
             description=description,
             language=language,
             datasource=datasource,
             rights=rights)
}


#' @keywords internal
new_dataset <- function (x,
                         DataBibentry,
                         datasubject) {

  validate_dataset(x = x,
                   DataBibentry = DataBibentry,
                   datasubject = datasubject)

  if (ncol(x) >0) {
    DataStructureNest <- lapply (1:ncol(x),
                                 function(col)
                                   initialise_dsd(df = x, col) )
    DSD <- DataStructureNest[[1]]
    if (ncol(x)>1) {
      for ( i in 2:ncol(x)) DSD <- c(DSD,  DataStructureNest[[i]])
    }


  } else {
    DSD <- list()
  }

  attr(x, "DataBibentry") <- DataBibentry
  attr(x, "Subject") <- datasubject
  attr(x, "DataStructure") <- DSD
  class(x) <- c("dataset", class(x))
  x
}

#' @keywords internal
validate_dataset <- function(x,
                             DataBibentry,
                             datasubject) {

  if (! inherits(x, "data.frame"))   {
    wrong_class <- class(x)
    stop("dataset(x=...) must be inherited from a data.frame (like data.frame, tibble, data.table, tsibble ...), not ", wrong_class)
  }

  if (! inherits(DataBibentry, "bibentry"))   {
    wrong_class <- class(DataBibentry)
    stop("dataset(Bibentry=...) must be inherited from a bibentry, not ", wrong_class)
  }

  if (! inherits(datasubject, "subject") )   {
    wrong_class <- class(subject)
    stop("dataset(subject=...) must be inherited from a subject [created by dataset::subject()], not ", wrong_class)
  }

}

#' @rdname dataset
#' @export
is.dataset <- function(x) inherits(x, "dataset")


#' @rdname dataset
#' @param n Number of rows to print.
#' @export
print.dataset <- function(x, n, ...) {

  args <- list(...)
  if (length(args)==0)  {
    args <- list(digits = NULL,
                 quote = FALSE,
                 right = TRUE,
                 row.names = TRUE,
                 max = NULL)
  }

  print(attr(x, "DataBibentry"))
  x_df <- as.data.frame(x)
  if(!missing(n)) {
    if ("row.names" %in% names(args)) {
      if (args$row.names == FALSE) {
        args$max <- min(dim(x)[1]*(dim(x)[2]), n*(dim(x)[2]))
      } else {
        args$max <- min(dim(x)[1]*(dim(x)[2]+1), n*(dim(x)[2]+1))
      }
    } else {
      args$max <- min(dim(x)[1]*(dim(x)[2]+1), n*(dim(x)[2]+1))
    }
  }

  print(x_df, digits=args$digits, row.names=args$row.names, max = args$max )
  cat(paste0("Further metadata: describe(", deparse(substitute(x)), ")\n"))
}

#' @rdname dataset
#' @param object an object for which a summary is desired.
#' @param ... Further arguments passed on to generic methods
#' like \code{summary(x, ...)}.
#' @export
summary.dataset <- function(object, ...) {
  args <- list(...)
  print(attr(object, "DataBibentry"))
  cat(paste0("Further metadata: describe(", deparse(substitute(object)), ")\n"))
  NextMethod()
 }



#' @keywords internal
`cbind.dataset` <- function(..., deparse.level = 1) {
  DataStructure <- attr(x, "DataStructure")
  DataBibentry <- dataset_bibentry(x)
  x_Subject <- subject(x)
  NextMethod()
  attr(x, "DataStructure") <- DataStructure
  attr(x, "DataBibentry") <- DataBibentry
  subject(x) <- x_Subject
  x
}





