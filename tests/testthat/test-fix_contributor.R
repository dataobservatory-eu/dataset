test_that("fix_contributors() works", {
  expect_equal(fix_contributor(contributors = "contributor"), "contributor")
  expect_equal(fix_contributor(contributors = NULL), ":unas")
  expect_equal(fix_contributor(contributors = person("Jane", "Doe", role = "ctb")), "Jane Doe [ctb]")
  expect_equal(fix_contributor(contributors = person("American Iris Society", role = "pbl")), "American Iris Society [pbl]")
  expect_equal(fix_contributor(contributors = c(person("Jane", "Doe", role = "ctb"),
                                                person("Daniel", "Antal",
                                                       comment = c(ORCID = "0000-0001-7513-6760")))),
               "{Jane Doe [ctb]} and {Daniel Antal (ORCID: <https://orcid.org/0000-0001-7513-6760>)}")
})
