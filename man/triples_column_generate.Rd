% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_to_triples.R
\name{triples_column_generate}
\alias{triples_column_generate}
\title{Internal: Generate RDF triples for a single column}
\usage{
triples_column_generate(s_vec, col, colname)
}
\arguments{
\item{s_vec}{A character vector of subject URIs (length = number of rows)}

\item{col}{The column vector (e.g., \code{x[[i]]})}

\item{colname}{The name of the column (used as fallback for predicate)}
}
\value{
A data.frame with columns s, p, o
}
\description{
Create subject-predicate-object triples from one column of a dataset
}
\keyword{internal}
