
test_that("agent()<- assignment works", {
  p1 <- person(given="Jane", family="Doe", role=c("cre", "aut"))
  p2 <- person(given="Joe", family= "Doe", role=c("aut", "dtm"))
  p3 <- person(given="Publisher Inc", role=c("pbl"))
  test_df <- dataset_df(data.frame(a=1, b=2))
  expect_equal(get_bibentry(test_df)$author, person("Author", "Unknown"))
  expect_error(agent(a=1)<-person("Jane Doe"))
  agent(x=test_df) <- c(p1,p2,p3)
})

test_that("agent() works", {
  p1 <- person("Jane", "Doe", role=c("cre", "aut"))
  p2 <- person("Joe", "Doe", role=c("aut", "dtm"))
  p3 <- person("Publisher Inc", role=c("pbl"))
  expect_equal(length(agent(x=c(p1, p2, p3))), 3)
  expect_equal(agent(x=c(p1, p2, p3))$creators, person("Jane", "Doe", role=c("cre", "aut")))
  expect_null(agent(x=c(p1, p2, p3))$contributors)
  expect_equal(agent(x=c(p1, p2, p3))$publisher, person("Publisher Inc", role=c("pbl")))
})


