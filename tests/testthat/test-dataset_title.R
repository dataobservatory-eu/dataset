

my_iris <- iris
my_iris <- title_add(my_iris,
                     Title = c("The Famous Dataset", "Iris Dataset"),
                     titleType = c("AlternativeTitle", "Title")
)



test_that("title_add works", {
  expect_equal(dataset_title(my_iris), list(Title = "Iris Dataset",
                                            AlternativeTitle = "The Famous Dataset"))
})
