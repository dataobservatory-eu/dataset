test_that("xsd_convert.data.frame() works", {
  expect_equal(xsd_convert(x=head(iris))[1,1], '\"5.1\"^^<xs:decimal>')
})


test_that("xsd_convert.dataset() works", {
  expect_equal(as.character(xsd_convert(iris_dataset)[1,1]),  '\"5.1\"^^<xs:decimal>')
})

test_that("xsd_convert.list() throws error", {
  expect_error(
    xsd_convert(x=list(a=c(1:4)))
    )
})


