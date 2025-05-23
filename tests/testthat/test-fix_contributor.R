test_that("returns :unas for NULL input", {
  expect_equal(fix_contributor(NULL), ":unas")
})

test_that("returns :unas for ':unas' input", {
  expect_equal(fix_contributor(":unas"), ":unas")
})

test_that("handles a single person with role", {
  p <- person(given = "Jane", family = "Doe", role = "dtm")
  expect_equal(fix_contributor(p), "{Jane Doe [dtm]}")
})

test_that("handles a list of persons with mixed roles", {
  p1 <- person(given = "Jane", family = "Doe", role = "dtm")
  p2 <- person(given = "John", family = "Smith")
  expect_equal(
    fix_contributor(list(p1, p2)),
    "{Jane Doe [dtm]} and {John Smith [ctb]}"
  )
})
