% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/provenance.R
\name{provenance}
\alias{provenance}
\alias{provenance<-}
\title{Get or update provenance information}
\usage{
provenance(x)

provenance(x) <- value
}
\arguments{
\item{x}{A dataset created with \code{\link{dataset_df}}.}

\item{value}{Use \code{\link{n_triples}} to add further statement values.}
}
\value{
\code{provenance(x)} returns the provenance attributes created by
\code{\link{n_triples}} as a text; \code{provenance(x)<-value} adds the new
provenance attributes and returns \code{x} invisibly.
}
\description{
Add or update information about the history (provenance) of the dataset.
}
\examples{
provenance(orange_df)

## add a statement:

 provenance(orange_df) <- n_triple(
  "https://doi.org/10.5281/zenodo.10396807",
  "http://www.w3.org/ns/prov#wasInformedBy",
  "http://example.com/source#1")
}
