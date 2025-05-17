
## get_type internal function -------------------------------------
test_that("get_type returns correct XSD types", {
  expect_equal(get_type(1.5), "xs:decimal")
  expect_equal(get_type(1L), "xs:integer")
  expect_equal(get_type("text"), "xs:string")
  expect_equal(get_type(TRUE), "xs:boolean")
  expect_equal(get_type(Sys.Date()), "xs:date")
  expect_equal(get_type(Sys.time()), "xs:dateTime")
  expect_equal(get_type(as.difftime(c("0:3:20", "11:23:15"))), "xs:duration")
})

## Character conversions ------------------------------------------
test_that("xsd_convert.character handles standard strings", {
  expect_equal(
    xsd_convert(c("apple", " banana ", "cherry")),
    c('"apple"^^<xs:string>', '" banana "^^<xs:string>', '"cherry"^^<xs:string>')
  )
})

test_that("xsd_convert.character handles NA values", {
  result <- xsd_convert(c("apple", NA, "cherry"))
  expect_equal(result[1], '"apple"^^<xs:string>')
  expect_true(is.na(result[2]))
  expect_equal(result[3], '"cherry"^^<xs:string>')
})

test_that("xsd_convert.character handles empty strings", {
  expect_equal(xsd_convert(""), '""^^<xs:string>')
})

test_that("xsd_convert.character handles empty vector", {
  expect_equal(xsd_convert(character(0)), '""^^<xs:string>')
})

## Numeric conversions ----------------------------------------------------

test_that("xsd_convert.numeric handles standard numeric values", {
  expect_equal(
    xsd_convert(c(1.0, 2.5, 3.0)),
    c('"1"^^<xs:decimal>', '"2.5"^^<xs:decimal>', '"3"^^<xs:decimal>')
  )
})

test_that("xsd_convert.numeric handles NA values", {
  result <- xsd_convert(c(1.0, NA, 2.0))
  expect_equal(result[1], '"1"^^<xs:decimal>')
  expect_true(is.na(result[2]))
  expect_equal(result[3], '"2"^^<xs:decimal>')
})

test_that("xsd_convert.numeric handles empty vector", {
  expect_equal(xsd_convert(numeric(0)), '""^^<xs:decimal>')
})

## Integer conversions ----------------------------------------------------
test_that("xsd_convert.integer handles normal integers", {
  expect_equal(xsd_convert(1:3), c('"1"^^<xs:integer>', '"2"^^<xs:integer>', '"3"^^<xs:integer>'))
})

test_that("xsd_convert.integer handles NA", {
  x <- as.integer(c(42, NA))
  result <- xsd_convert(x)
  expect_equal(result[1], '"42"^^<xs:integer>')
  expect_true(is.na(result[2]))
})

test_that("xsd_convert.integer handles empty input", {
  expect_equal(xsd_convert(integer(0)), '""^^<xs:integer>')
})


## Factor conversions -----------------------------------------------------
test_that("xsd_convert.factor handles standard factors without codelist", {
  f <- factor(c("apple", "banana", "cherry"))
  result <- xsd_convert(f)
  expect_equal(result, c(
    '"apple"^^<xs:string>',
    '"banana"^^<xs:string>',
    '"cherry"^^<xs:string>'
  ))
})

test_that("xsd_convert.factor handles factors with NA values", {
  f <- factor(c("apple", NA, "cherry"))
  result <- xsd_convert(f)
  expect_equal(result[1], '"apple"^^<xs:string>')
  expect_true(is.na(result[2]))
  expect_equal(result[3], '"cherry"^^<xs:string>')
})

test_that("xsd_convert.factor handles factors with codelist", {
  f <- factor(c("apple", "banana", "cherry"))
  result <- xsd_convert(f, codelist = "fruit")
  expect_equal(result, c(
    "fruit:apple",
    "fruit:banana",
    "fruit:cherry"
  ))
})

test_that("xsd_convert.factor handles factors with codelist and NA values", {
  f <- factor(c("apple", NA, "cherry"))
  result <- xsd_convert(f, codelist = "fruit")
  expect_equal(result[1], "fruit:apple")
  expect_true(is.na(result[2]))
  expect_equal(result[3], "fruit:cherry")
})

test_that("xsd_convert.factor handles empty factor", {
  f <- factor(character(0))
  result <- xsd_convert(f)
  expect_equal(result, '""^^<xs:string>')
})

## Logical conversions -----------------------------------------------------

test_that("xsd_convert.logical handles TRUE and FALSE correctly", {
  expect_equal(xsd_convert(TRUE), '"true"^^<xs:boolean>')
  expect_equal(xsd_convert(FALSE), '"false"^^<xs:boolean>')
})

test_that("xsd_convert.logical returns NA for NA input", {
  expect_true(is.na(xsd_convert(NA)))
  expect_equal(
    xsd_convert(c(TRUE, NA, FALSE)),
    c('"true"^^<xs:boolean>', NA, '"false"^^<xs:boolean>')
  )
})

## Date conversions ------------------------------------------------------
test_that("xsd_convert.Date formats correctly", {
  d <- as.Date("2023-05-01")
  expect_equal(xsd_convert(d), '"2023-05-01"^^<xs:date>')
})

test_that("xsd_convert.Date handles NA", {
  d <- as.Date(NA)
  expect_equal(xsd_convert(d), NA_character_)
})

test_that("xsd_convert.Date handles empty input", {
  d <- as.Date(character(0))
  expect_equal(xsd_convert(d), '""^^<xs:date>')
})

test_that("xsd_convert.Date works when coerced from POSIXct with timezone", {
  dt <- as.POSIXct("2020-01-01 12:00:00", tz = "Europe/Berlin")
  d <- as.Date(dt)
  expect_equal(xsd_convert(d), '"2020-01-01"^^<xs:date>')
})


test_that("xsd_convert.POSIXct converts datetime to xs:dateTime in UTC", {
  ts <- as.POSIXct("2020-01-01 12:34:56", tz = "UTC")
  expect_equal(xsd_convert(ts), '"2020-01-01T12:34:56Z"^^<xs:dateTime>')
})

test_that("xsd_convert.POSIXct handles POSIXct in local timezone correctly", {
  ts_local <- as.POSIXct("2020-01-01 12:34:56", tz = Sys.timezone())
  result <- xsd_convert(ts_local)

  # Convert to expected UTC
  ts_utc <- format(ts_local, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expected <- paste0('"', ts_utc, '"^^<xs:dateTime>')

  expect_equal(result, expected)
})

## POSIXct conversions ---------------------------------------------------
test_that("xsd_convert.POSIXct handles a vector of times", {
  ts <- as.POSIXct(c("2020-01-01 12:00:00", "2021-01-01 15:30:00"), tz = "UTC")
  expect_equal(
    xsd_convert(ts),
    c('"2020-01-01T12:00:00Z"^^<xs:dateTime>', '"2021-01-01T15:30:00Z"^^<xs:dateTime>')
  )
})

test_that("xsd_convert.POSIXct handles NA datetime values", {
  ts <- as.POSIXct(NA, tz = "UTC")
  expect_equal(xsd_convert(ts), NA_character_)
})


test_that("xsd_convert.POSIXct handles empty vector", {
  ts <- as.POSIXct(character(0))
  expect_equal(xsd_convert(ts), '""^^<xs:dateTime>')
})

## data.frame conversions ----------------------------------

test_that("xsd_convert.data.frame handles various column types", {
  df <- data.frame(
    id = 1:2,
    name = c("apple", NA),
    price = c(1.50, 2.30),
    available = c(TRUE, NA),
    date = as.Date(c("2020-01-01", NA)),
    stringsAsFactors = FALSE
  )

  result <- xsd_convert(df, idcol = "id")

  expect_equal(result$name[1], '\"apple\"^^<xs:string>')
  expect_true(is.na(result$name[2]))
  expect_equal(result$price[2], '"2.3"^^<xs:decimal>')
  expect_equal(result$id, as.character(c(1L, 2L)))
})
#' @rdname xsd_convert
#' @export
#' @exportS3Method
xsd_convert.data.frame <- function(x, idcol = NULL, ...) {
  # Identify ID column (or default to row names)
  if (!is.null(idcol)) {
    id_idx <- idcol_find(x, idcol)
    ids <- as.character(x[[id_idx]])
    id_name <- names(x)[id_idx]
  } else {
    ids <- row.names(x)
    id_name <- ".rowid"
  }

  convert_cols <- setdiff(seq_along(x), if (!is.null(idcol)) id_idx else integer(0))

  # Apply xsd_convert to all non-ID columns
  xsd_list <- lapply(convert_cols, function(c) xsd_convert(x[[c]], ...))
  names(xsd_list) <- names(x)[convert_cols]

  # Assemble result: ID column first, then converted columns
  result <- c(
    setNames(list(ids), id_name),
    xsd_list
  )

  as.data.frame(result, stringsAsFactors = FALSE)
}

## dataset_df  ---------------------------------------------
test_that("xsd_convert handles dataset_df", {
  orange_xsd <- xsd_convert(orange_df, idcol = "rowid")
  expect_equal(orange_xsd$rowid[1:3], c("orange:1", "orange:2", "orange:3"))
  expect_equal(orange_xsd$age[1], '"118"^^<xs:decimal>')
})

## Empty strings ---------------------------------------------
test_that("xsd_convert handles empty vectors semantically", {
  expect_equal(xsd_convert(character(0)), '""^^<xs:string>')
  expect_equal(xsd_convert(numeric(0)), '""^^<xs:decimal>')
  expect_equal(xsd_convert(logical(0)), '""^^<xs:boolean>')
})

## Conversion of difftime ----------------------------------------
test_that("xsd_convert.difftime handles hours and seconds", {
  x <- as.difftime(c(3600, 5400), units = "secs")
  result <- xsd_convert(x)
  expect_equal(result, c('"PT1H"^^<xs:duration>', '"PT1H30M"^^<xs:duration>'))
})

test_that("xsd_convert.difftime handles NA values", {
  x <- as.difftime(c(3600, NA, 7200), units = "secs")
  result <- xsd_convert(x)
  expect_equal(result[1], '"PT1H"^^<xs:duration>')
  expect_true(is.na(result[2]))
  expect_equal(result[3], '"PT2H"^^<xs:duration>')
})

test_that("xsd_convert.difftime handles empty input", {
  x <- as.difftime(numeric(0), units = "secs")
  expect_equal(xsd_convert(x), '""^^<xs:duration>')
})
