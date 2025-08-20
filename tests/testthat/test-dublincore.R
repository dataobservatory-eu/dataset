test_that("creates valid dublincore object", {
  dc <- dublincore(
    title = "Sample Dataset",
    creator = person("Jane", "Doe")
  )
  expect_s3_class(dc, c("dublincore", "bibrecord", "bibentry"))
  expect_equal(dc$title, "Sample Dataset")
  expect_true(inherits(dc$author[[1]], "person"))
})

test_that("dublincore() errors when title has length > 1", {
  expect_error(
    dublincore(
      title = c("Too", "Many", "Titles"),
      creator = person("Jane", "Doe")
    ),
    "title must be a single character string"
  )
})


test_that("missing creator fails", {
  expect_error(
    dublincore(title = "Missing Creator"),
    "creator.*required"
  )
})

test_that("default values are set", {
  dc <- dublincore(
    title = "Defaults Example",
    creator = person("Jane", "Doe")
  )
  expect_equal(dc$rights, ":tba")
  expect_equal(dc$format, "application/r-rds")
  expect_equal(dc$relation, ":unas")
})

test_that("custom contributor is set", {
  contrib <- person(given = "Antal", family = "Daniel", role = "dtm")
  dc <- dublincore(
    title = "Contrib Example",
    creator = person("Jane", "Doe"),
    contributor = contrib
  )
  expect_equal(attr(dc, "contributor")[[1]]$given, "Antal")
  expect_equal(attr(dc, "contributor")[[1]]$role, "dtm")
})

test_that("dataset_date sets year correctly", {
  dc <- dublincore(
    title = "Date Example",
    creator = person("Jane", "Doe"),
    dataset_date = as.Date("2022-06-01")
  )
  expect_equal(dc$year, "2022")
})

test_that("identifier, language, and format are preserved", {
  dc <- dublincore(
    title = "Complete Example",
    creator = person("Jane", "Doe"),
    identifier = "https://doi.org/10.1234/test",
    language = "en",
    dataset_format = "application/json"
  )
  expect_equal(dc$identifier, "https://doi.org/10.1234/test")
  expect_equal(dc$language, "en")
  expect_equal(dc$format, "application/json")
})

test_that("print.dublincore prints key fields", {
  s1 <- subject_create("Climate Change", schemeURI = "http://id.loc.gov/subjects", subjectScheme = "LCSH")

  dc <- dublincore(
    title = "Climate Data",
    creator = person("Eve", "Rivera", role = "cre"),
    publisher = "Climate Org",
    subject = s1,
    description = "A dataset on climate change indicators."
  )

  expect_output(print(dc), "Dublin Core Metadata Record")

  out <- capture_output(print(dc))

  # Header
  expect_match(out, "Dublin Core Metadata Record")
  expect_match(out, "--------------------------")

  # Fields
  expect_match(out, "Title:\\s+Climate Data")
  expect_match(out, "Creator\\(s\\):\\s+Eve Rivera \\[cre\\]")
  expect_match(out, "Subject\\(s\\):\\s+Climate Change")
  expect_match(out, "Publisher:\\s+Climate Org")
  expect_match(out, "Year:\\s+:tba")
  expect_match(out, "Description:\\s+A dataset on climate change indicators")

  # Alignment: all labels should have the colon at same position
  lines <- strsplit(out, "\n")[[1]]
  meta_lines <- grep(":", lines, value = TRUE)

  # find where values start (first non-space after colon)
  value_start <- vapply(meta_lines, function(l) {
    m <- regexpr(":", l)
    rest <- substr(l, m + 1, nchar(l))
    m + regexpr("[^ ]", rest)
  }, integer(1))

  expect_true(length(unique(value_start)) == 1)
})

test_that("is.dublincore returns TRUE for dublincore object", {
  dc <- dublincore(
    title = "Dublin Check",
    creator = person("Jane", "Doe")
  )
  expect_true(is.dublincore(dc))
})

test_that("is.dublincore returns FALSE for non-dublincore object", {
  df <- data.frame(x = 1:3)
  expect_false(is.dublincore(df))
})

test_that("dublincore stores structured relation as attribute and
          flat relation in slot", {

   rel <- related_create("https://doi.org/10.5678/def", "References", "DOI")
   dc <- dublincore(
              title="X",
              creator=person("A","B", role="cre"),
              relation = rel
            )
   expect_equal(dc$relation, "https://doi.org/10.5678/def")
   expect_s3_class(attr(dc, "relation"), "related", exact = FALSE)
})
