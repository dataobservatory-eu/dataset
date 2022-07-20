

related_item_type = "dataset"
related_item_identifier <-  list (
  scheme = "url",
  identifier =  "https://zenodo.org/record/5703222#.YZYkm2DMLIU",
  relation =  "compiles",
  resource_type =  "dataset"
)

creator = person ("Daniel", "Antal", role = "aut")
title = "Dataset title"
publisher = "zenodo"
publication_year = 2022


related_item_identifer ()

related_item <- function(x) {

}


rel_item_1 <- list (
  scheme = "url",
  identifier = "https://zenodo.org/record/5704568#.YZYkc2DMLIU",
  relation = "compiles",
  resource_type = "dataset")
rel_item_2 <- list (
  scheme = "url",
  identifier =  "https://zenodo.org/record/5703222#.YZYkm2DMLIU",
  relation =  "compiles",
  resource_type =  "dataset"
)


list  ( "my_rec"= rel_item_1, "eurostat_2021"= rel_item_2)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
