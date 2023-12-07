test_ds <- dataset(data.frame( a = 1:3,
                          b = 4:6),
              title = "Test Dataset",
              author = c(
                person(family ="Doe",
                       given ="Jane",
                       role = "aut")
              ),
              year = "2023",
              version = "0.1")

test_that("creator() works", {
  expect_error(creator(test_ds) <- "hello")
  expect_equal(creator(test_ds), person("Jane", "Doe", role = "aut"))
})


creator(test_ds, overwrite = TRUE) <- person("Joe", "Doe")

test_that("creator(.., overwrite = TRUE) works", {
  expect_equal(creator(test_ds), person("Joe", "Doe"))
})

creator(test_ds, overwrite = FALSE) <- person("Jane", "Doe")
test_that("creator(..., overwrite = FALSE) works", {
  expect_equal(creator(test_ds), c(person("Joe", "Doe"),
                                   (person("Jane", "Doe")))
)
})

