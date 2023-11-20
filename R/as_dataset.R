
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

  if (is.null(subject)) subject <- new_Subject("")

  new_dataset(x,
              DataBibentry = DataBibentry,
              subject = subject)

}
