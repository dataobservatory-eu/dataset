
related_item_identifier(Identifier = "https://zenodo.org/record/5703222#.YZYkm2DMLIU",
                        relatedIdentifierType = "DOI",
                        relationType = "CompiledBy",
                        schemeURI = "URI",
                        resourceTypeGeneral = "Dataset"
)


my_rel_item  <- related_item (Identifier = "https://zenodo.org/record/5703222#.YZYkm2DMLIU",
                              Creator = person ("Daniel", "Antal", role = "aut"),
                              Publisher = "Zenodo",
                              PublicationYear = 2022,
                              relatedIdentifierType = "DOI",
                              relationType = "CompiledBy",
                              schemeURI = "URI",
                              resourceTypeGeneral = "Dataset")


test_that("rel_item works", {
  expect_equal(my_rel_item$Publisher, "Zenodo" )
  expect_equal(my_rel_item$PublicationYear, 2022 )
})
