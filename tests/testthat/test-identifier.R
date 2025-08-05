test_that("identifier() assignment and retrieval works:", {
  a <- dublincore(
    title = "Test",
    creator = person("Person", "Unknown"),
    identifier = c(DOI = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
  )

  expect_equal(
    identifier(a),
    "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
  )

  identifier(x = a, overwrite = TRUE) <- "https://doi.org/10.1111/"
  expect_equal(identifier(a), "https://doi.org/10.1111/")
  expect_equal(a$doi, "10.1111")
})

test_that("identifier overwriting works", {
  test_df <- dataset_df(
    a = defined(1:2, label = "test"),
    dataset_bibentry = dublincore(
      title = "Test",
      creator = person("Person", "Unknown"),
      identifier = c(DOI = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
    )
  )
  expect_equal(
    identifier(test_df),
    "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
  )
  identifier(test_df) <- NULL
  expect_equal(identifier(test_df), ":unas")
  identifier(test_df) <- 1234
  expect_equal(identifier(test_df), "1234")
})

test_that("identifier() uses unas placeholder", {
  iris_dataset_2 <- iris_dataset
  identifier(iris_dataset_2) <- NULL
  expect_equal(identifier(iris_dataset_2), ":unas")
})

test_that("identifier()<- assignment works", {
  iris_dataset_2 <- iris_dataset
  identifier(iris_dataset_2) <- "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
  expect_equal(identifier(iris_dataset_2), "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x")
})
