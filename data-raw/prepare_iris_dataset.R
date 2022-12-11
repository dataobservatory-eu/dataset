iris_dataset <- dataset(
  x = iris,
  Dimensions = NULL,
  Measures = c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width" ),
  Attributes = "Species",
  Title = "Iris Dataset"
)

dataset_title(iris_dataset)

iris_dataset <- dublincore_add(
  x = iris_dataset,
  Creator = person("Edgar", "Anderson", role = "aut"),
  Publisher = "American Iris Society",
  Source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
  Date = 1935,
  Language = "en",
  Description = "This famous (Fisher's or Anderson's) iris data set."
)
dataset_title(iris_dataset)
print(iris_dataset)
usethis::use_data(iris_dataset, overwrite = TRUE)

#snakecase::to_title_case("Edgar Anderson's Iris Data (For Testing the dataset R package)")
dataset_export(ds=my_iris_dataset, file = file.path("not_included", "iris_dataset.csv"))
saveRDS(iris_dataset, file.path("not_included", "iris_dataset.rds"))
write.csv(my_iris_dataset, file = file.path("not_included", "iris.csv"), row.names = FALSE)
