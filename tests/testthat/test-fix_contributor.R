test_that("fix_contributor() handles edge cases correctly", {

  # Single person, one role
  expect_equal(
    fix_contributor(person(given = "Jane", family = "Doe", role = "dtm")),
    "{Jane Doe [dtm]}"
  )

  # Single person, no role
  expect_equal(
    fix_contributor(person(given = "Jane", family = "Doe")),
    "{Jane Doe [ctb]}"
  )

  # Person with multiple roles
  expect_equal(
    fix_contributor(c(
      person(given = "Jane", family = "Doe", role = "dtm"),
      person(given = "Jane", family = "Doe", role = "ctb")
    )),
    "{Jane Doe [dtm, ctb]}"
  )

  # Multiple persons, no duplicates
  expect_equal(
    fix_contributor(c(
      person(given = "Jane", family = "Doe", role = "dtm"),
      person(given = "John", family = "Smith", role = "ctb")
    )),
    "{Jane Doe [dtm]} and {John Smith [ctb]}"
  )

  # Contributor with no family name (institution)
  expect_equal(
    fix_contributor(person(given = "World Health Organization", role = "dtm")),
    "{World Health Organization [dtm]}"
  )

  # Contributor list is NULL
  expect_equal(
    fix_contributor(NULL),
    ":unas"
  )

  # Contributor list is :unas
  expect_equal(
    fix_contributor(":unas"),
    ":unas"
  )

  # Contributor with NULL role (should not add empty brackets)
  expect_equal(
    fix_contributor(person(given = "Jane", family = "Doe", role = NULL)),
    "{Jane Doe [ctb]}"
  )

})


