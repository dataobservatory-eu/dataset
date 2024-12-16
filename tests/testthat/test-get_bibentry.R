

test_that("get_bibentry() works", {
  iris_bibentry <- get_bibentry(iris_dataset)
  expect_equal(iris_bibentry$title, "Iris Dataset")
  expect_equal(iris_bibentry$year, "1935")
})

test_that("create_bibentry() works", {
  expect_equal(create_bibentry(reference=list(title="Test", author=person(given="Jane", family="Doe")))$title, "Test")
  expect_error(create_bibentry(reference=list(title="Test", author=person(given="Jane", family="Doe"),
                                              contributor ="author name string")))
  expect_equal(create_bibentry(reference=list(title="Test", author=person(given="Jane", family="Doe"), geolocation="AD"))$geolocation, "AD")
})

test_that("set_bibentry() works", {
  expect_equal(create_bibentry(reference=
                                 list(title="Test",
                                      author=person(given="Jane",
                                                    family="Doe")))$year, substr(as.character(Sys.time()),1,4))
  expect_true(is.null(create_bibentry(reference= list(title="Test",
                                                      author=person(given="Jane",
                                                                    family="Doe")))$date))
  expect_true(is.null(create_bibentry(reference= list(title="Test",
                                                      author=person(given="Jane",
                                                                    family="Doe")))$datasource))
  expect_true(is.null(create_bibentry(reference= list(title="Test",
                                                      author=person(given="Jane",
                                                                    family="Doe")))$coverage))
  expect_true(is.null(create_bibentry(reference= list(title="Test",
                                                      author=person(given="Jane",
                                                                    family="Doe")))$source))
  expect_equal(get_bibentry(set_bibentry(mtcars, NULL))$author, person("Unknown", "Author"))
  expect_equal(create_bibentry(reference= list(title="Test",
                                               author=person(given="Jane",
                                                             family="Doe"),
                                               datasource="test"))$source, "test")
  expect_equal(create_bibentry(reference= list(title="Test",
                                               author=person(given="Jane",
                                                             family="Doe"),
                                               type="dataset"))$type, "dataset")
})




