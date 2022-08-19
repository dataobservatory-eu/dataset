test_that("dataset_title_create() works", {
  expect_error(dataset_title_create("My title") <- list(sex="F"))
  expect_error(dataset_title_create("hello", c(1:2)))
  expect_equal(dataset_title_create("hello")$Title, "hello")
  expect_equal(dataset_title_create("hello")$titleType, "Title")
  expect_error(dataset_title_create("hello", "Unknown"))
  expect_equal(dataset_title_create(c("My Title", "My Subtitle"), c("Title", "Subtitle"))$titleType, c("Title", "Subtitle"))
})

y <- data.frame()
dataset_title(y) <- "This is a title"

test_that("dataset_title() works", {
  expect_equal(dataset_title(y)$Title, "This is a title")
  expect_equal(dataset_title(y)$titleType, "Title")
})

dataset_title(y) <- dataset_title_create("This is a subtitle", titleType = "Subtitle")

test_that("dataset_title() adds new types of titles", {
  expect_equal(dataset_title(y)$Title, c("This is a title", "This is a subtitle"))
  expect_equal(dataset_title(y)$titleType, c("Title", "Subtitle"))
})

dataset_title(y) <- dataset_title_create("This is an alternative title", titleType = "AlternativeTitle")

test_that("dataset_title() adds new types of titles", {
   expect_equal(dataset_title(y)$titleType, c("Title", "Subtitle", "AlternativeTitle"))
})

test_that("dataset_title() throws error", {
  expect_error(dataset_title_create("This is a wrong title", titleType = "WrongTitle"))
})



