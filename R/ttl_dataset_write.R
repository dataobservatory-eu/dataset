#' @title Write a dataset into Turtle serialisation
#' @description
#' Write a dataset into a Turtle serialisation.
#'
#' @param tdf A dataset in exactly three columns.
#' @param ttl_namespace The namespace definitions of the dataset.
#' @param file_path The path to the file that should be written or appended.
#' @param overwrite If the file exists, overwrite it? Defaults to \code{TRUE}.
#' @return A text file with the prefix and the observation serialisations.
#' @examples
#' testtdf <- data.frame (s = c("eg:o1", "eg:01", "eg:02"),
#'                        p = c("a", "eg-var:", "eg-var"),
#'                        o = c("qb:Observation",
#'                              "\"1\"^^<xs:decimal>",
#'                              "\"2\"^^<xs:decimal>"))
#'
#' examplefile <- file.path(tempdir(), "ttl_dataset_write.ttl")
#'
#' dataset_ttl_write(tdf=testtdf, file_path = examplefile)
#'
#' readLines(examplefile)
#' @export

dataset_ttl_write <- function(tdf,
                              ttl_namespace = NULL,
                              file_path = NULL,
                              overwrite = TRUE) {

  ## load dataset_namespace
  default_namespace <- getdata("dataset_namespace")
  default_namespace <- default_namespace[
    which(default_namespace$prefix %in% c("rdf:", "rdfs:", "owl:",
                                          "qb:", "dcat:", "xsd:")),]

  ## validate dataset
  validate_tdf(tdf)

  ## use subject predicate, object names
  names(tdf) <- c("s", "p", "o")

  if (is.null(file_path)) file_path <- file.path(tempdir(), "tmp.ttl")

  if (is.null(ttl_namespace)) {
    ttl_namespace <- default_namespace
  }

  ttl_prefix_write(ttl_namespace = ttl_namespace,
                   file_path=file_path,
                   overwrite=overwrite)

  invisible(ttl_observations_write(tdf, file_path))
}

#' @keywords internal
ttl_prefix_write <- function(ttl_namespace, file_path, overwrite) {
  if ( file.exists(file_path) & overwrite ) file.remove(file_path)

  create_string <- paste(
    apply ( ttl_namespace, 1, function(x) {
      prefix <- x[[1]]
      n <- max(12-nchar(x[[1]]), 1)
      separator <- paste(rep (" ", n), collapse="")
      paste0("@prefix  ", prefix, separator, x[[2]], " .")
    }), sep = "", collapse = "\n"
  )

  write(create_string, file=file_path, append = F )
}

#' @keywords internal
ttl_observations_write <- function(tdf, file_path) {
  ## tdf must be s, o, p
  write("\n# -- Observations -----------------------------------------\n",
        file = file_path, append = TRUE)

  lapply ( unique(tdf$s), function(x) {
    observation <- tdf[which(tdf$s == x), ]
    write( c(unique(paste0(observation$s, " a qb:Observation ;")),
             paste0("   ", observation$p, "   ", observation$o, " ;"),
             "   ."),
           file = file_path, append = T
    )
  })
}

#' @keywords internal
validate_tdf <- function(tdf) {

  if ( ! inherits(tdf, "data.frame") ) {
    stop("ttl_dataset_write(): tdf must be a data.frame (inherited) object")
  }

  if ( ncol(tdf) != 3 ) {
    stop("ttl_dataset_write(): tdf must have exactly three columns.")
  }

  TRUE
}
