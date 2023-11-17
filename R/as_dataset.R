
#' @rdname dataset
#' @export
as_dataset <- function(x,
                       author,
                       title,
                       publisher = NULL,
                       year = NULL,
                       identifier = NULL,
                       description = NULL,
                       version = NULL,
                       subject = NULL,
                       language = NULL) {
  UseMethod("as_dataset", x)
}

#' @rdname dataset
#' @importFrom utils bibentry
#' @export
as_dataset.data.frame <- function(x,
                                  author,
                                  title,
                                  publisher = NULL,
                                  year = NULL,
                                  identifier = NULL,
                                  description = NULL,
                                  version = NULL,
                                  subject = NULL,
                                  language = NULL) {

  assertthat::assert_that(inherits(author, "person"),
                          msg="The person person must be created with utils::person().")

  if (is.null(year)) year <- substr(as.character(Sys.Date()), 1,4)
  if (is.null(version)) version  <- "0.1.0" else version <- as.character(version)
  if (is.null(publisher)) publisher <- ":tba"
  if (is.null(identifier)) identifier <- ":tba"
  if (is.null(description)) description <- ":unas"
  if (is.null(language)) language <- ":unas"
  if (is.null(subject)) subject <- new_Subject(NULL)

  DataBibentry  <- utils::bibentry(bibtype="Misc",
                                   title = title,
                                   author = author,
                                   publisher = publisher,
                                   year = year,
                                   resourceType = "Dataset",
                                   identifier = identifier,
                                   version = version,
                                   description  = description,
                                   language = language
  )

  new_dataset(x,
              DataBibentry = DataBibentry,
              subject = subject)

}
