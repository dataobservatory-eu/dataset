
test_that("xsd_convert(x, idcols) works", {
  expect_equal(xsd_convert(TRUE), "\"TRUE\"^^<xs:boolean>")
  expect_equal(xsd_convert(defined (x = 2:4, label = "test")), c('\"2\"^^<xs:integer>', '\"3\"^^<xs:integer>', '\"4\"^^<xs:integer>'))
  expect_equal(xsd_convert(head(iris)[1,1]), '\"5.1\"^^<xs:decimal>')
  expect_equal(unlist(xsd_convert(x=head(iris_dataset), idcol=1)[1,2]), c(Sepal.Length = '\"5.1\"^^<xs:decimal>' ))
})


