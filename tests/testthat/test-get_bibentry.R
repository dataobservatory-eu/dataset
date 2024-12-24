alternative_bibentry <- datacite(
  Creator=person("Jane Doe"),
  Title ="The Famous Iris Dataset",
  Publisher = "MyOrg")

test_that("get_bibentry() works", {
  expect_error(get_bibentry(iris))
  iris_bibentry <- get_bibentry(iris_dataset)
  expect_equal(iris_bibentry$title, "Iris Dataset")
  expect_equal(iris_bibentry$date, "1935")
})

test_that("set_bibentry() works", {
  iris_dataset_2 <- iris_dataset
  new_bibentry <- dublincore(title="Test", creator=person("Jane", "Doe"), dataset_date=2013)
  set_bibentry(dataset=iris_dataset_2) <- new_bibentry
  expect_equal(get_bibentry(iris_dataset_2)$title, "Test")
})



