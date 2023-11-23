#' @inheritParams utils:bibentry
#' @keywords internal
#' @importFrom assertthat assert_that
#' @importFrom utils bibentry
as_bibentry <- function(bibtype="Misc",
                        title,
                        author, ... ) {

  assertthat::assert_that(inherits(author, "person"),
                          msg="The author must be created with utils::person().")

  arguments <- list(...)

  year <- ifelse (is.null(arguments$year), substr(as.character(Sys.Date()), 1,4), as.character(arguments$year))
  version <- ifelse (is.null(arguments$version), "0.1.0", as.character(arguments$version))
  publisher <-  ifelse (is.null(arguments$publisher), ":tba", as.character(arguments$publisher))
  identifier <-  ifelse (is.null(arguments$identifier), ":tba", as.character(arguments$identifier))
  description <-  ifelse (is.null(arguments$description), as.character(":unas"), as.character(arguments$description))
  language <-  ifelse (is.null(arguments$language), ":unas", as.character(arguments$language))
  subject <-  ifelse (is.null(arguments$subject), new_Subject(NULL), arguments$subject)
  format <-  ifelse (is.null(arguments$format), "application/r-rds", arguments$format)

  tmp <- bibentry(bibtype=bibtype,
                  title = title,
                  author = author,
                  publisher = publisher,
                  year = year,
                  resourceType = "Dataset",
                  identifier = identifier,
                  version = version,
                  description  = description,
                  language = language,
                  format = format)

  if (!is.null(arguments$format)) tmp$format <- arguments$format
  if (!is.null(arguments$rights)) tmp$rights <- arguments$right
  if (!is.null(arguments$contributor)) {
    assertthat::assert_that(inherits(arguments$contributor, "person"),
                            msg="The contributor must be created with utils::person().")

    tmp$author <- c(tmp$author, arguments$contributor)
  }

  if (!is.null(arguments$date)) tmp$date <- arguments$date
  if (!is.null(arguments$type)) tmp$type <- arguments$type
  if (!is.null(arguments$coverage)) tmp$coverage <- arguments$coverage
  if (!is.null(arguments$datasource)) tmp$source <- arguments$datasource
  if (!is.null(arguments$geolocation)) tmp$geolocation <- arguments$geolocation
  if (!is.null(arguments$fundingreference)) tmp$fundingreference <- arguments$fundingreference

  tmp
}

