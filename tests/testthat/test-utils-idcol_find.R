test_that("idcol_find() works", {
  expect_equal(idcol_find(iris_dataset, "rowid"), 1)
})

test_that("idcol_find() works", {
  mtcars2 <- mtcars
  mtcars2$rowid <- 1:nrow(mtcars)
  expect_equal(idcol_find(mtcars2, "rowid"), 12)
  expect_error(idcol_find(mtcars2, "bowid"))
})



