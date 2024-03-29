% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ttl_dataset_write.R
\name{dataset_ttl_write}
\alias{dataset_ttl_write}
\title{Write a dataset into Turtle serialisation}
\usage{
dataset_ttl_write(
  tdf,
  ttl_namespace = NULL,
  file_path = NULL,
  overwrite = TRUE
)
}
\arguments{
\item{tdf}{A dataset in exactly three columns.}

\item{ttl_namespace}{The namespace definitions of the dataset.}

\item{file_path}{The path to the file that should be written or appended.}

\item{overwrite}{If the file exists, overwrite it? Defaults to \code{TRUE}.}
}
\value{
A text file with the prefix and the observation serialisations.
}
\description{
Write a dataset into a Turtle serialisation.
}
\examples{
testtdf <- data.frame (s = c("eg:o1", "eg:01", "eg:02"),
                       p = c("a", "eg-var:", "eg-var"),
                       o = c("qb:Observation",
                             "\"1\"^^<xs:decimal>",
                             "\"2\"^^<xs:decimal>"))

examplefile <- file.path(tempdir(), "ttl_dataset_write.ttl")

dataset_ttl_write(tdf=testtdf, file_path = examplefile)

readLines(examplefile)
}
