my_item <- related_item (Identifier = "https://zenodo.org/record/5703222#.YZYkm2DMLIU",
                         Creator = person ("Daniel", "Antal", role = "aut"),
                         Publisher = "Zenodo",
                         PublicationYear = 2022,
                         relatedIdentifierType = "DOI",
                         relationType = "CompiledBy",
                         schemeURI = "URI",
                         resourceTypeGeneral = "Dataset")


test_that("related_item() works", {
  expect_equal(my_item$Publisher, "Zenodo")
})
