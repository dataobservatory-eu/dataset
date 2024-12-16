
qid = defined(c("Q275912", "Q116196078"), namespace = "https://www.wikidata.org/wiki/")


test_that("var_namespace() works", {
  expect_true(is.defined(qid))
  expect_equal(var_namespace(qid),"https://www.wikidata.org/wiki/")
})


test_that("set_namespace_attribute() works", {
  expect_equal(
    get_namespace_attribute(set_namespace_attribute(qid, "https://www.wikiba.se/")), "https://www.wikiba.se/"
  )
  expect_error(set_namespace_attribute(qid, c("https://www.wikidata.org/wiki/", "https://www.wikiba.se/")))
})

var_namespace(qid) <- "https://www.wikibase.se/"
test_that("`var_namespace<-` works", {
  expect_equal(get_namespace_attribute(qid), "https://www.wikibase.se/")
  expect_error(var_namespace(qid) <-  c("https://www.wikidata.org/wiki/", "https://www.wikiba.se/"))
})


