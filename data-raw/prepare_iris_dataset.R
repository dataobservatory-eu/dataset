data(iris)
eg_iris <- iris
iris_doi <- "10.5281/zenodo.10396807"

iris_dataset <- dataset_df(
  rowid = defined(paste0("#", row.names(iris)),
                  label = "ID in the iris dataset",
                  namespace="10.5281/zenodo.10396807"),
  Sepal.Length = defined(eg_iris$Sepal.Length,
                         label = "Length of the sepal in cm",
                         unit = "centimeter",
                         definition = "https://www.wikidata.org/wiki/Property:P2043"),
  Petal.Length = defined(eg_iris$Petal.Length,
                         label = "Length of the petal in cm",
                         unit = "centimeter",
                         definition = "https://www.wikidata.org/wiki/Property:P2043"),
  Sepal.Width = defined(eg_iris$Sepal.Width,
                        label = "Width of the sepal in cm",
                        unit = "centimeter",
                        definition = "https://www.wikidata.org/wiki/Property:P2049"),
  Petal.Width = defined(eg_iris$Petal.Width,
                        label = "Width of the petal in cm",
                        unit = "centimeter",
                        definition  = "https://www.wikidata.org/wiki/Property:P2049"),
  Species = defined(eg_iris$Species,
                    label = "Taxon name within the Iris genus",
                    definition = "https://npgsweb.ars-grin.gov/gringlobal/taxon/taxonomygenus?id=6074",
                    namespace = "Iris" ),
  dataset_bibentry =  dublincore(
    title = "Iris Dataset",
    creator = person(given="Edgar", family="Anderson", role = "aut"),
    identifier = "https://doi.org/10.5281/zenodo.10396807",
    publisher = "American Iris Society",
    datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
    date = 1935,
    language = "en",
    description = "The famous (Fisher's or Anderson's) iris data set."
  ),
  subject = subject_create(term  = "Irises (plants)",
                           schemeURI = "http://id.loc.gov/authorities/subjects",
                           valueURI = "https://id.loc.gov/authorities/subjects/sh85068079",
                           subjectScheme = "LCCH",
                           prefix = "lcch:")
)

subject(iris_dataset)
print(get_bibentry(iris_dataset), "Bibtex")
attributes(iris_dataset$Species)
get_bibentry(iris_dataset)

var_definition(iris_dataset$Sepal.Length)
#DSD <- DataStructure(iris_dataset)
#DSD$Sepal.Length$label <- "The sepal length of iris specimen in centimeters."
#DSD$Petal.Length$label <- "The petal length of iris specimen in centimeters."
##DSD$Sepal.Width$label <- "The sepal width of iris specimen in centimeters."
#DSD$Petal.Width$label <- "The petal width of iris specimen in centimeters."
#DSD$Species$label <- "The iris species of the observed plant."
#DataStructure_update(x = iris_dataset, value=DSD)

#describe(iris_dataset)
#attr(iris_dataset, "DataStructure") <- DSD
prov <- n_triples(
  c(n_triple("https://doi.org/10.5281/zenodo.10396807", "a", "http://purl.org/linked-data/cube#DataSet"),
    n_triple("https://orcid.org/0000-0001-7513-6760", "a", "http://www.w3.org/ns/prov#Agent"),
    n_triple("https://doi.org/10.5281/zenodo.6703764.", "a", "http://www.w3.org/ns/prov#SoftwareAgent")
  )
)

attr(iris_dataset, "prov") <- prov
usethis::use_data(iris_dataset, overwrite = TRUE)

#snakecase::to_title_case("Edgar Anderson's Iris Data (For Testing the dataset R package)")
#saveRDS(iris_dataset, file.path("not_included", "iris_dataset.rds"))

#csv_iris_dataset <- tibble::rowid_to_column(iris_dataset)
#csv_iris_dataset$rowid <- paste0("doi:10.5281/zenodo.10396807:o", csv_iris_dataset$rowid)


#write.csv(csv_iris_dataset, file = file.path("not_included", "iris_dataset.csv"), row.names = FALSE)
#save_attributes_to_file <- attributes(csv_iris_dataset)
#save_attributes_to_file$row.names <- NULL


person_to_list <- function(p) lapply ( dataset_bibentry(p), function(x) ifelse (inherits(x, "person"), as.list(x), x) )

bibentry_to_list <- function(x) {
  x <- ifelse (inherits(x, "bibentry"), as.list(person_to_list(x)), x)
}

#attributes_file <- file.path("not_included", "iris_attributes.json")
#writeLines(jsonlite::toJSON(lapply (save_attributes_to_file, bibentry_to_list )),
#           con = attributes_file)

temp_dir <- tempdir()
tempcon <- file.path(temp_dir, "iris_dataset.bib")
writeLines(text = paste(
  format(get_bibentry(dataset=iris_dataset), "Bibtex"),
  collapse = "\n\n"),
  con = tempcon )

readLines(tempcon)



attr(iris_dataset, "prov") <- prov
provenance(iris_dataset)
get_bibentry(iris_dataset)


iris_dataset

var_label(iris_dataset)
var_unit(iris_dataset$Sepal.Length)


#readRDS(file.path("not_included",  "iris_dataset.rds"))

#readRDS(file.path("not_included",  "iris_dataset.csv"))
