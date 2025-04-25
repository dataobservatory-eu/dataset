test_that("fix_publisher() works", {
  expect_equal(fix_publisher(publishers = "publisher"), "publisher")
  expect_equal(fix_publisher(publishers = NULL), ":unas")
  expect_equal(fix_publisher(publishers = person("Jane", "Doe", role = "pbl")),
               "Jane Doe")
  expect_equal(fix_publisher(publishers = person("American Iris Society", role = "pbl")), "American Iris Society")
  expect_equal(fix_publisher(publishers = c(
    person("Jane", "Doe", role = "pbl"),
    person("American Iris Society")
  )), "{Jane Doe} and {American Iris Society}")
})
