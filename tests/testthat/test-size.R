my_object <- data.frame ( a = 1:100,
                          b = 1001:1100)

test_that("size() works", {
  expect_true(grepl("KiB", attr(size(my_object), "Size")))
})
