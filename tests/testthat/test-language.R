
test_that("language() works", {
  expect_equal(language(iris_dataset), "en")
})

myiris <- iris_dataset
value <- "fr"

test_that("language() works", {
  language(x=myiris) <- "fr"
  expect_equal(language(myiris), "fra")
})


test_that("language() works", {
  language(x=myiris, iso_639_code = "639-1") <- "fr"
  expect_equal(language(myiris), "fr")
})

test_that("language() works", {
  language(x=myiris, iso_639_code = "639-1") <- "FR"
  expect_equal(language(myiris), "fr")
})

test_that("language()<- throws error", {
  expect_error(language(x=myiris) <- "hello")
})
