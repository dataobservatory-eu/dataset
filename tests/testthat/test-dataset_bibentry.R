ds <- dataset(iris,
        title = "The iris Dataset",
        author = c(
          person(family ="Anderson",
                 given ="Edgar",
                 role = "aut")
           ),
         identifier = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
         year = "1935",
         version = "1.0",
         description = "The famous dataset that is distributed with R.",
         url = "https://en.wikipedia.org/wiki/Iris_flower_data_set",
         resourceType = "Dataset"
         )

test_that("multiplication works", {
  expect_equal(class(dataset_bibentry(ds)), "bibentry")
})
