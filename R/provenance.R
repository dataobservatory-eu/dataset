#' @title Get or update provenance information
#' @description Add or update information about the history (provenance) of the dataset.
#' @param x A dataset created with \code{\link{dataset_df}}.
#' @param value Use \code{\link{n_triples}} to add further statement values.
#' @examples
#' provenance(iris_dataset)
#'
#' ## add a statement:
#'  provenance(iris_dataset) <- n_triple(
#'   "https://doi.org/10.5281/zenodo.10396807",
#'   "http://www.w3.org/ns/prov#wasInformedBy",
#'   "http://example.com/source#1")
#' @return The provenance statements of the dataset.
#' @export
provenance <- function(x) {
  if(!is.dataset_df(x)) {
    stop("provenance(x): x must be a dataset_df object with standardised provenance metadata.")
  }
  attr(x, "prov")
}

#' @rdname provenance
#' @export
`provenance<-` <- function(x,  value) {

  if (!is.dataset_df(x)) {
    stop("provenance(x)<- : x must be a dataset object created with dataset() or as_dataset().")
  }

  old_provenance <- provenance(x)
  new_provenance <- old_provenance

  attr(x, "prov") <- c(new_provenance, value)
  invisible(x)
}

#' @keywords internal
default_provenance <- function(dataset_id = "http://example.com/dataset#",
                               creator_id =NULL,
                               started_at_time = started_at_time,
                               ended_at_time = ended_at_time) {
  cite_dataset <- citation("dataset")
  if(is.null(creator_id)) creator_statement <- NULL else {
    creator_statement <- n_triple(creator_id, "a", "http://www.w3.org/ns/prov#Agent")
  }
  prov <- n_triples(
    c(n_triple(dataset_id, "a", "http://purl.org/linked-data/cube#DataSet"),
      creator_statement,
      n_triple("http://example.com/creation", "a", "http://www.w3.org/ns/prov#Activity"),
      n_triple("http://example.com/creation", "http://www.w3.org/ns/prov#startedAtTime", xsd_convert(started_at_time) ),
      n_triple("http://example.com/creation", "http://www.w3.org/ns/prov#endedAtTime", xsd_convert(ended_at_time) ),
      n_triple(paste0("https://doi.org/", cite_dataset[[2]]$doi), "a", "http://www.w3.org/ns/prov#SoftwareAgent")
    )
  )
  prov
}


