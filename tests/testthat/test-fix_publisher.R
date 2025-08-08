test_that("fix_publisher() works", {
  expect_equal(fix_publisher(publishers = "publisher"), "publisher")
  expect_equal(fix_publisher(publishers = NULL), ":unas")
  expect_equal(
    fix_publisher(publishers = person("Jane", "Doe", role = "pbl")),
    "Jane Doe"
  )
  expect_equal(fix_publisher(publishers = person("Open Data Institute", role = "pbl")), "Open Data Institute")
  expect_equal(fix_publisher(publishers = c(
    person("Jane", "Doe", role = "pbl"),
    person("Open Data Institute")
  )), "{Jane Doe} and {Open Data Institute}")
})
