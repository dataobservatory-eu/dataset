iris_dataset <- dataset(
  x = iris,
  title = "Iris Dataset",
  author = person("Edgar", "Anderson", role = "aut"),
  publisher = "American Iris Society",
  datasource = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  year = 1935,
  language = "en",
  description = "The famous (Fisher's or Anderson's) iris data set."
)

describe(iris_dataset)
usethis::use_data(iris_dataset, overwrite = TRUE)

#snakecase::to_title_case("Edgar Anderson's Iris Data (For Testing the dataset R package)")
saveRDS(iris_dataset, file.path("not_included", "iris_dataset.rds"))
write.csv(iris_dataset, file = file.path("not_included", "iris.csv"), row.names = FALSE)
tempcon <- file.path("not_included", "iris.bib")
writeLines(text = paste(
  format(dataset_bibentry(ds=iris_dataset), "Bibtex"),
  collapse = "\n\n"),
  con = tempcon )

readLines(tempcon)

usethis::use_data(iris_dataset, overwrite=TRUE)
