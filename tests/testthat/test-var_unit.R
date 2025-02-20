test_that("var_unit()", {
  expect_equal(var_unit(iris$Sepal.Length), NULL)
})

test_that("var_unit()", {
  var_unit(iris$Sepal.Length) <- "centimeters"
  expect_equal(var_unit(iris$Sepal.Length), "centimeters")
  expect_error(var_unit(iris$Sepal.Length) <- c("cm", "mm"),
    regexp = "should be a single character string or NULL"
  )
})

test_that("unit_attribute()", {
  var_unit(iris$Sepal.Length) <- "centimeters"
  expect_equal(unit_attribute(iris$Sepal.Length), "centimeters")
  expect_equal(get_unit_attribute(iris$Sepal.Length), "centimeters")
})
