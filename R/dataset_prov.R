#' @title Create a dataset provenance
#'
#' @description Create a Turtle format description of the dataset's provenance.
#'
#' @inheritParams dataset
#' @param x dataset object created by [dataset()] or [as_dataset()].
#' @param start_time Starting time of the dataset creation process.
#' @param end_time Ending time fo the dataset creation process.
#' @return A triple-format data.frame with the provenance information.
#' @importFrom utils citation
#' @keywords internal

dataset_prov <- function(x= NULL,
                         dataset_id = "eg:dataset#",
                         author,
                         start_time = Sys.time(),
                         end_time=Sys.time()) {

  cite_dataset <- citation("dataset")
  author_curie <- paste0(dataset_id, "author")
  process_curie <- paste0(dataset_id, "creation")

  if (!is.null(x) & is.dataset(x)) {
    provenance_info <- provenance(x)

    start_time <- provenance_info$started_at
    end_time <- provenance_info$ended_at
    was_associated_with <- provenance_info$wasAssocitatedWith
    author <- creator(x)
  } else {
    was_associated_with <- paste0("doi:", unlist(cite_dataset$doi))
  }

  dataset_creation <- data.frame(
    type = c("prov:Activity"),
    startedAt = xsd_convert(start_time),
    endedAt = xsd_convert(end_time),
    wasStartedBy = author_curie,
    wasAssocitatedWith = was_associated_with
  )

  dataset_package <- data.frame(
    type = c("prov:SoftwareAgent"),
    label = "dataset R package",
    actedOnBehalf = author_curie
  )

  author_prov <- data.frame(
    type = c("prov:Agent"),
    label = xsd_convert(as.character(author)),
    givenName = xsd_convert(author[[1]]$given),
    family_name = xsd_convert(author[[1]]$family)
  )

  author_prov2 <- data.frame(
    type = c("prov:Person")
  )

  tmp <- rbind(
    dataset_to_triples(
      id_to_column(dataset_creation, prefix = process_curie, ids = ""),
      idcol = "rowid"
    ),
    dataset_to_triples(
      id_to_column(dataset_package,
                   prefix = paste0("doi:", unlist(cite_dataset$doi)),
                                   ids = ""),
      idcol = "rowid"
    ),

    dataset_to_triples(
      id_to_column(author_prov2, prefix = author_curie, ids = ""),
      idcol = "rowid"
    ),
    dataset_to_triples(
      id_to_column(author_prov, prefix = author_curie, ids = ""),
      idcol = "rowid"
    )
  )
  tmp
}



#' @keywords internal
provenance_add  <- function(x = NULL,
                            start_time = NULL,
                            end_time = NULL,
                            associated_with = NULL,
                            informed_by = NULL) {

  existing_prov <- NULL
  names(attributes(x))
  existing_prov <- attr(x, "Provenance")

  uri_dataset_package <- paste0("doi:", unlist(citation("dataset")$doi))

  if (is.null(associated_with)) {
    associated_with <- uri_dataset_package
  } else {
    associated_with <- c(associated_with, uri_dataset_package)
  }

  tmp <- list (
    started_at = as.POSIXct(start_time, tz = "Europe/London"),
    ended_at   = as.POSIXct(end_time, tz = "Europe/London"),
    wasAssocitatedWith = associated_with )

  if ( !is.null(informed_by)) {
    tmp <- c(tmp, list (wasInformedBy = informed_by))
  }

  tmp
}

