

test_that("id_to_column works ()", {
  expect_true(is.dataset_df(head(id_to_column(x=iris_dataset))))
  expect_true(is.data.frame(head(id_to_column(iris))))
  expect_equal(head(id_to_column(x=iris, prefix="eg:iris-o" ), 3)$rowid, paste0("eg:iris-o", 1:3))
  expect_equal(head(id_to_column(x=iris, prefix=NULL ), 3)$rowid, as.character(1:3))
  expect_equal(names(id_to_column(mtcars))[1], "rowid")
  test <- id_to_column(x=dataset_df(mtcars, reference=list(author = person("Jane", "Doe"))))
  expect_equal(get_bibentry(test)$author, person("Jane", "Doe"))
})




