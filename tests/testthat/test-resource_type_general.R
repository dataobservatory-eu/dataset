my_rel_item  <- related_item (Identifier = "https://zenodo.org/record/5703222#.YZYkm2DMLIU",
                              Creator = person ("Daniel", "Antal", role = "aut"),
                              Publisher = "Zenodo",
                              PublicationYear = 2022,
                              relatedIdentifierType = "DOI",
                              relationType = "CompiledBy",
                              schemeURI = "URI",
                              resourceTypeGeneral = "Dataset")

test_that("resource_type_genera() works", {
  expect_equal(resource_type_general(my_rel_item), "Dataset")
  expect_error(resource_type_general(my_rel_item) <- "Wings")
})
