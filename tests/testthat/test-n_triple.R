triple_1 <- n_triple("http://example.org/show/218", "http://www.w3.org/2000/01/rdf-schema#label", "That Seventies Show")
triple_2 <- n_triple("http://example.org/show/218", "http://example.org/show/localName",  '"Cette Série des Années Septante"@fr-be')


author_person <- person(given="Daniel", family="Antal", comment = c(ORCID = "https://orcid.org/0000-0001-7513-6760"))

test_that("n_triples()", {
  expect_equal(triple_1, "<http://example.org/show/218> <http://www.w3.org/2000/01/rdf-schema#label> \"That Seventies Show\"^^<http://www.w3.org/2001/XMLSchema#string> .")
  expect_equal(length(n_triples(c(triple_1, triple_2, triple_1))), 2)
  expect_equal(length(n_triples(c(triple_1, triple_2))), 2)
  expect_equal(n_triple("https://orcid.org/0000-0001-7513-6760", "a",  'http://www.w3.org/ns/prov#Agent'),
               '<https://orcid.org/0000-0001-7513-6760> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> .')
  expect_equal(create_iri(23), '\"23\"^^<http://www.w3.org/2001/XMLSchema#double>')
  expect_equal(create_iri("23"), '\"23\"^^<http://www.w3.org/2001/XMLSchema#string>')
  expect_equal(create_iri(as.integer(23)), '\"23\"^^<http://www.w3.org/2001/XMLSchema#integer>')
  expect_equal(create_iri(as.Date("2024-10-30")), '\"2024-10-30\"^^<http://www.w3.org/2001/XMLSchema#date>')
  expect_equal(prov_author(author_person), "\"_:antaldaniel\" <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> .")
  expect_equal(get_orcid(author_person), c(ORCID = "https://orcid.org/0000-0001-7513-6760"))
})


test_that("create_iri()", {
  expect_error(create_iri(list(a=1:2)))
  expect_equal(create_iri(2), "\"2\"^^<http://www.w3.org/2001/XMLSchema#double>")
  expect_true(grepl('http://www.w3.org/2001/XMLSchema#date>', create_iri(Sys.Date()) ))
})

author_person <- person(given = "Daniel", family = "Antal",
                        email = "daniel.antal@dataobservatory.eu",
                        role = c("aut", "cre"),
                        comment = c(ORCID = "0000-0001-7513-6760")
)

test_that("get_orcid()", {
  expect_equal(get_orcid(author_person), c(ORCID = "0000-0001-7513-6760"))
  expect_equal(get_orcid(person("Jane Doe")), NULL)
})

#prov_author("Jane Doe")
