test_that("contributor() returns only contributor persons", {
  df <- dataset_df(data.frame(x = 1))
  creator(df) <- person("Alice", "Smith", role = "aut")
  contributor(df, overwrite = FALSE) <- person("Infra",
    role = "ctb",
    comment = c(contributorType = "hostingInstitution")
  )
  cons <- contributor(df)
  chr <- vapply(cons, as.character, character(1))
  expect_true(any(grepl("Infra.*\\[ctb\\]", chr)))
  # ensure creators not included
  all_people <- vapply(creator(df), as.character, character(1))
  expect_true(any(grepl("Alice Smith", all_people)))
  expect_false(any(grepl("Alice Smith", chr)))
})

test_that("contributor() returns only
          contributors (role = 'ctb' or contributorType)", {
  df <- dataset_df(data.frame(x = 1))
  creator(df) <- utils::person("Alice", "Author", role = "aut")
  creator(df, overwrite = FALSE) <- utils::person("Bob", "Builder", role = "ctb")
  creator(df, overwrite = FALSE) <- utils::person("Carol", "Curator", comment = c(contributorType = "DataCurator"))

  ctb <- contributor(df)

  expect_true(all(vapply(ctb, function(p) {
    identical(p$role, "ctb") ||
      (!is.null(p$comment) && !is.null(p$comment[["contributorType"]]))
  }, logical(1))))
})

test_that("contributor() returns empty when no contributors", {
  df <- dataset_df(data.frame(x = 1))
  creator(df) <- utils::person("Alice", "Author", role = "aut")
  expect_length(contributor(df), 0)
})

test_that("contributor<-() adds contributor and sets role to 'ctb' if missing", {
  df <- dataset_df(data.frame(x = 1))
  creator(df) <- utils::person("Alice", "Author", role = "aut")

  contributor(df) <- utils::person("Jane", "Doe")
  ctb <- contributor(df)

  expect_equal(length(ctb), 1)
  expect_equal(ctb[[1]]$role, "ctb")
})

test_that("contributor<-() appends when overwrite = FALSE", {
  df <- dataset_df(data.frame(x = 1))
  contributor(df) <- utils::person("Jane", "Doe", role = "ctb")
  contributor(df, overwrite = FALSE) <- utils::person("John", "Smith", role = "ctb")

  ctb <- contributor(df)
  expect_equal(length(ctb), 2)
})

test_that("contributor<-() overwrites contributors when overwrite = TRUE", {
  df <- dataset_df(data.frame(x = 1))
  contributor(df) <- utils::person("Jane", "Doe", role = "ctb")
  contributor(df, overwrite = TRUE) <- utils::person("John", "Smith", role = "ctb")

  ctb <- contributor(df)
  expect_equal(length(ctb), 1)
  expect_equal(ctb[[1]]$given, "John")
})

test_that("contributor<-() errors on wrong type", {
  df <- dataset_df(data.frame(x = 1))
  expect_error(contributor(df) <- "not a person", "utils::person object")
})

test_that("contributor<-() errors if role is not 'ctb'", {
  df <- dataset_df(data.frame(x = 1))
  expect_error(contributor(df) <- utils::person("Jane", "Doe", role = "aut"), "role must be 'ctb'")
})
