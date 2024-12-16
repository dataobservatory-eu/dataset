#' @title Get/set the Bibentry of the object.
#' @param dataset A dataset created with \code{\link{dataset_df}}.
#' @param value A \code{\link[utils:bibentry]{utils::bibentry}} object, or a newly
#' initialised bibentry object with DataCite default values for unassigned entries.
#' @importFrom utils bibentry
#' @importFrom utils person
#' @return A \code{\link[utils]{bibentry}} bibliographic entry created for the dataset.
#' @examples
#' get_bibentry(iris_dataset)
#' @export

get_bibentry <- function(dataset) {
    attr(dataset, "dataset_bibentry")
}

#' @keywords internal
create_bibentry <- function(reference) {

  bibtype <- "Misc"
  author      <- ifelse (is.null(reference$author), person("Unknown Author"), reference$author)
  title       <- ifelse (is.null(reference$title), person("Unknown Dataset"), as.character(reference$title))
  year        <- ifelse (is.null(reference$year), substr(as.character(Sys.Date()), 1,4), as.character(reference$year))
  version     <- ifelse (is.null(reference$version), "0.1.0", as.character(reference$version))
  publisher   <-  ifelse (is.null(reference$publisher), ":unas", as.character(reference$publisher))
  identifier  <-  ifelse (is.null(reference$identifier), ":tba", as.character(reference$identifier))
  doi         <-  ifelse (is.null(reference$doi), "", as.character(reference$doi))
  description <-  ifelse (is.null(reference$description), as.character(":unas"), as.character(reference$description))
  language    <-  ifelse (is.null(reference$language), ":unas", as.character(reference$language))
  #subject     <-  ifelse (is.null(reference$subject), NULL, reference$subject)
  format      <-  ifelse (is.null(reference$format), "application/r-rds", reference$format)
  rights      <-  ifelse (is.null(reference$rights), ":unas", as.character(reference$rights))

 if(!is.null(doi)&identifier==":tba") identifier <- doi

  author <-reference$author
  remove_null_elements <- function(x) {
    x[sapply(x, is.null)] <- NULL
  }

  for ( i in seq_along(author)) remove_null_elements(author[[1]])

  #author[[1]][sapply(author[[1]], is.null)] <- NULL

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
                  format = format,
                  rights = rights)

  if (!is.null(reference$format)) tmp$format <- reference$format
  if (!is.null(reference$contributor)) {
    if (! inherits(reference$contributor, "person")) {
      stop ("The contributor must be created with utils::person().")
    }
    tmp$author <- c(tmp$author, reference$contributor)
  }
  if (nchar(doi)>0) tmp$doi <- reference$doi
  if (!is.null(reference$date)) tmp$date <- reference$date
  if (!is.null(reference$type)) tmp$type <- reference$type
  if (!is.null(reference$coverage)) tmp$coverage <- reference$coverage
  if (!is.null(reference$datasource)) tmp$source <- reference$datasource
  if (!is.null(reference$geolocation)) tmp$geolocation <- reference$geolocation
  if (!is.null(reference$fundingreference)) tmp$fundingreference <- reference$fundingreference

  tmp
}

#' @rdname get_bibentry
#' @export
set_bibentry <- function(dataset, value) {

  sys_time <- Sys.time()
  year <- substr(as.character(sys_time),1,4)

  if(is.null(value)) {
    value <- list(title="Untitled Dataset",
                      author="Unknown Author")
  }

  if(is.null(value$year)) value$year <- year

  dataset_bibentry <- create_bibentry(value)

  attr(dataset, "dataset_bibentry") <- dataset_bibentry

  invisible(dataset)
}

