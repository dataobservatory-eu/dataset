% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataStructure.R
\name{DataStructure}
\alias{DataStructure}
\alias{DataStructure_update}
\title{Data structure}
\usage{
DataStructure(x)

DataStructure_update(x, value)
}
\arguments{
\item{x}{A dataset.}

\item{value}{A DataStructure definition.}
}
\value{
A dataset object, which is a data.frame or inherited object with rich
metadata.
}
\description{
Read or update the Data Structure Definition of a dataset.
}
\examples{
DataStructure(iris_dataset)
dsd_iris <- DataStructure(iris_dataset)
dsd_iris$Sepal.Length$label <- "The sepal length measured in centimeters."
dsd_iris$Sepal.Width$label  <- "The sepal width measured in centimeters."
dsd_iris$Petal.Length$label <- "The petal length measured in centimeters."
dsd_iris$Petal.Width$label  <- "The petal width measured in centimeters."
dsd_iris$Species$label      <- "The name of the Iris species in the observation."

iris_dataset_labelled <- DataStructure_update(iris_dataset, dsd_iris)

vapply(DataStructure(iris_dataset_labelled), function(x) x$label, character(1))
}
