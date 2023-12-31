data(iris)
eg_iris <- iris
iris_doi <- "doi:10.5281/zenodo.10396807"
row.names(eg_iris) <- paste0("iris:o", row.names(iris))

iris_dataset <- dataset(
  x = eg_iris,
  title = "Iris Dataset",
  author = person("Edgar", "Anderson", role = "aut"),
  identifier = "https://doi.org/10.5281/zenodo.10396807",
  publisher = "American Iris Society",
  datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  year = 1935,
  language = "en",
  description = "The famous (Fisher's or Anderson's) iris data set."
)

DSD <- DataStructure(iris_dataset)
DSD$Sepal.Length$label <- "The sepal length of iris specimen in centimeters."
DSD$Petal.Length$label <- "The petal length of iris specimen in centimeters."
DSD$Sepal.Width$label <- "The sepal width of iris specimen in centimeters."
DSD$Petal.Width$label <- "The petal width of iris specimen in centimeters."
DSD$Species$label <- "The iris species of the observed plant."
DataStructure_update(x = iris_dataset, value=DSD)

describe(iris_dataset)
attr(iris_dataset, "DataStructure") <- DSD
usethis::use_data(iris_dataset, overwrite = TRUE)

#snakecase::to_title_case("Edgar Anderson's Iris Data (For Testing the dataset R package)")
saveRDS(iris_dataset, file.path("not_included", "iris_dataset.rds"))

csv_iris_dataset <- tibble::rowid_to_column(iris_dataset)
csv_iris_dataset$rowid <- paste0("doi:10.5281/zenodo.10396807:o", csv_iris_dataset$rowid)


write.csv(csv_iris_dataset, file = file.path("not_included", "iris_dataset.csv"), row.names = FALSE)
save_attributes_to_file <- attributes(csv_iris_dataset)
save_attributes_to_file$row.names <- NULL


person_to_list <- function(p) lapply ( dataset_bibentry(p), function(x) ifelse (inherits(x, "person"), as.list(x), x) )

bibentry_to_list <- function(x) {
  x <- ifelse (inherits(x, "bibentry"), as.list(person_to_list(x)), x)
}

attributes_file <- file.path("not_included", "iris_attributes.json")
writeLines(jsonlite::toJSON(lapply (save_attributes_to_file, bibentry_to_list )),
           con = attributes_file)


tempcon <- file.path("not_included", "iris_dataset.bib")
writeLines(text = paste(
  format(dataset_bibentry(x=iris_dataset), "Bibtex"),
  collapse = "\n\n"),
  con = tempcon )

readLines(tempcon)

usethis::use_data(iris_dataset, overwrite=TRUE)


readRDS(file.path("not_included",  "iris_dataset.rds"))

readRDS(file.path("not_included",  "iris_dataset.csv"))
