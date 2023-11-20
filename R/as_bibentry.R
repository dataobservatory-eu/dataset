#' @inheritParams utils:bibentry
#' @keywords internal
#' @importFrom assertthat assert_that
#' @importFrom utils bibentry
as_bibentry <- function(x,
                        bibtype="Misc",
                        title,
                        author, ... ) {

  assertthat::assert_that(inherits(author, "person"),
                          msg="The person person must be created with utils::person().")

  arguments <- list(...)

  year <- ifelse (is.null(arguments$year), substr(as.character(Sys.Date()), 1,4), as.character(arguments$year))
  version <- ifelse (is.null(arguments$version), "0.1.0", as.character(arguments$version))
  publisher <-  ifelse (is.null(arguments$publisher), ":tba", as.character(arguments$publisher))
  identifier <-  ifelse (is.null(arguments$identifier), ":tba", as.character(arguments$identifier))
  description <-  ifelse (is.null(arguments$description), as.character(":unas"), as.character(arguments$description))
  language <-  ifelse (is.null(arguments$language), ":unas", as.character(arguments$language))
  subject <-  ifelse (is.null(arguments$subject), new_Subject(NULL), arguments$subject)

  bibentry(bibtype=bibtype,
           title = title,
           author = author,
           publisher = publisher,
           year = year,
           resourceType = "Dataset",
           identifier = identifier,
           version = version,
           description  = description,
           language = language)

}

