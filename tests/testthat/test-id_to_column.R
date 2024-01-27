

test_that("id_to_column works ()", {
  expect_true(is.dataset(head(id_to_column(x=iris_dataset))))
  expect_true(is.data.frame(head(id_to_column(iris))))
  expect_equal(head(id_to_column(iris_dataset), 3)$rowid, paste0("eg:iris-o", 1:3))
  expect_equal(head(id_to_column(x=iris, prefix="eg:iris-o" ), 3)$rowid, paste0("eg:iris-o", 1:3))
})


