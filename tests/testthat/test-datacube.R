dont_test <- function() {

  iris_dc <- as.datacube ( x = iris[1:6,],
                           obs_id = NULL,
                           dimension_names = NULL,
                           measure_names = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                           attribute_names = "Species")

  test_that("as.datacube", {
    expect_equal(attributes(iris_dc)$observations, "obs_id")
    expect_equal(length(attributes(iris_dc)$dimensions), 0)
    expect_equal(attributes(iris_dc)$attributes, "Species")
    expect_equal(attributes(iris_dc)$measures, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
  })

}
