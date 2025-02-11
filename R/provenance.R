#' @title Get or update provenance information
#' @description Add or update information about the history (provenance) of the dataset.
#' @param x A dataset created with \code{\link{dataset_df}}.
#' @param value Use \code{\link{n_triples}} to add further statement values.
#' @return \code{provenance(x)} returns the provenance attributes created by
#' \code{\link{n_triples}} as a text; \code{provenance(x)<-value} adds the new
#' provenance attributes and returns \code{x} invisibly.
#' @importFrom utils citation
#' @examples
#' provenance(iris_dataset)
#'
#' ## add a statement:
#'
#' provenance(iris_dataset) <- n_triple(
#'   "https://doi.org/10.5281/zenodo.10396807",
#'   "http://www.w3.org/ns/prov#wasInformedBy",
#'   "http://example.com/source#1"
#' )
#' @export
provenance <- function(x) {
  if (!is.dataset_df(x)) {
    stop("provenance(x): x must be a dataset_df object with standardised provenance metadata.")
  }
  attr(x, "prov")
}

#' @rdname provenance
#' @export
`provenance<-` <- function(x, value) {
  if (!is.dataset_df(x)) {
    stop("provenance(x)<- : x must be a dataset object created with dataset_df() or as_dataset_df().")
  }

  old_provenance <- provenance(x)
  new_provenance <- old_provenance

  attr(x, "prov") <- c(new_provenance, value)
  invisible(x)
}

#' @keywords internal
default_provenance <- function(dataset_id = "http://example.com/dataset#",
                               author = NULL,
                               dtm = NULL,
                               generated_at_time = NULL) {
  cite_dataset <- utils::citation("dataset")

  agent_triples <- prov_author(author)
  if ((!is.null(dtm))) c(agent_triples, prov_author(dtm))

  if (is.null(generated_at_time)) generated_at_time <- Sys.time()
  bundle_id <- gsub("#", "_prov.nt", dataset_id)
  prov <- n_triples(
    c(
      n_triple(bundle_id, "a", "http://www.w3.org/ns/prov#Bundle"),
      n_triple(dataset_id, "a", "http://www.w3.org/ns/prov#Entity"),
      n_triple(dataset_id, "a", "http://purl.org/linked-data/cube#DataSet"),
      n_triple(dataset_id, "a", "http://purl.org/linked-data/cube#DataSet"),
      agent_triples,
      n_triple("https://doi.org/10.32614/CRAN.package.dataset", "a", "http://www.w3.org/ns/prov#SoftwareAgent"),
      n_triple("http://example.com/creation", "a", "http://www.w3.org/ns/prov#Activity"),
      n_triple("http://example.com/creation", "http://www.w3.org/ns/prov#generatedAtTime", generated_at_time),
      n_triple(paste0("https://doi.org/", cite_dataset[[2]]$doi), "a", "http://www.w3.org/ns/prov#SoftwareAgent")
    )
  )

  prov
}
