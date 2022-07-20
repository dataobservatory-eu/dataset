x <- data.frame( geo = c("AL", "MK"),
                 value = c(1,2))
y <- subject_add (x, term = c("R (Computer program language)",
                         "Questionnaires--Computer programs"),
                scheme = rep("url", 2),
                identifier = c("https://id.loc.gov/authorities/subjects/sh2002004407.html",
                               "http://id.worldcat.org/fast/1085693/") )

test_that("multiplication works", {
  expect_equal(subjects(y), data.frame (term = c("R (Computer program language)",
                                                 "Questionnaires--Computer programs"),
                                                scheme = c("url", "url"),
                                                identifier = c("https://id.loc.gov/authorities/subjects/sh2002004407.html",
                                                              "http://id.worldcat.org/fast/1085693/")
                                        ) )
})
