

test_that("var_unit()", {
  expect_equal(var_unit(iris$Sepal.Length), NULL)
})

var_unit(iris$Sepal.Length) <- "centimeters"

test_that("var_unit()", {
  expect_equal(var_unit(iris$Sepal.Length), "centimeters")
  expect_error(var_unit(iris$Sepal.Length) <- c("cm", "mm"))
})


test_that("unit_attribute()", {
  expect_equal(unit_attribute(iris$Sepal.Length), "centimeters")
  expect_equal(get_unit_attribute(iris$Sepal.Length), "centimeters")
})
