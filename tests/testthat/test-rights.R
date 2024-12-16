

test_that("rights() works", {
  myiris <- iris_dataset
  expect_equal(rights(iris_dataset), ":unas")
  expect_error(rights(myiris) <- c(":unas", "hello"))
})

test_that("rights() works", {
  myiris <- iris_dataset
  iris_dataset2 <- myiris
  expect_equal(rights(myiris), ":unas")
  expect_equal(class(rights(iris_dataset2)), "character")
  expect_equal(rights(myiris, FALSE) <- "CC0", "CC0")  #change :unas is not an overwrite
  rights(x=myiris) <- NULL
  expect_equal(rights(myiris), ":unas")
})

# expect_equal(rights(iris_dataset2),  "CC-BY-SA")
#   rights(iris_dataset2, overwrite=TRUE) <- "CC-BY-SA"
#  expect_message(rights(myiris, FALSE) <- "CC1")
