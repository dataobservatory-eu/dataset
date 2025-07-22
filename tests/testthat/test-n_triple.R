test_that("n_triple and related helpers produce correct N-Triple strings", {
  # String literal object
  triple_1 <- n_triple(
    "http://example.org/show/218",
    "http://www.w3.org/2000/01/rdf-schema#label",
    "That Seventies Show"
  )

  # Language-tagged literal object
  triple_2 <- n_triple(
    "http://example.org/show/218",
    "http://example.org/show/localName",
    '"Cette Série des Années Septante"@fr-be'
  )

  # XSD datetime literal from POSIXct
  triple_time <- n_triple(
    "http://example.com/creation",
    "http://www.w3.org/ns/prov#generatedAtTime",
    as.POSIXct(10000, origin = "2024-01-01", tz = "UTC")
  )

  # ORCID agent as subject
  triple_agent <- n_triple(
    "https://orcid.org/0000-0001-7513-6760",
    "a",
    "http://www.w3.org/ns/prov#Agent"
  )

  # Person object with ORCID
  author_person <- person(
    given = "Daniel",
    family = "Antal",
    comment = c(ORCID = "https://orcid.org/0000-0001-7513-6760")
  )

  # Expectations
  expect_equal(triple_1,
               '<http://example.org/show/218> <http://www.w3.org/2000/01/rdf-schema#label> "That Seventies Show"^^<http://www.w3.org/2001/XMLSchema#string> .'
  )

  expect_equal(triple_time,
               '<http://example.com/creation> <http://www.w3.org/ns/prov#generatedAtTime> "2024-01-01T03:46:40Z"^^<xs:dateTime> .'
  )

  expect_equal(length(n_triples(c(triple_1, triple_2, triple_1))), 2)
  expect_equal(length(n_triples(c(triple_1, triple_2))), 2)

  expect_equal(triple_agent,
               '<https://orcid.org/0000-0001-7513-6760> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> .'
  )

  # create_iri() datatype coercion checks
  expect_equal(create_iri(23), '"23"^^<http://www.w3.org/2001/XMLSchema#double>')
  expect_equal(create_iri("23"), '"23"^^<http://www.w3.org/2001/XMLSchema#string>')
  expect_equal(create_iri(as.integer(23)), '"23"^^<http://www.w3.org/2001/XMLSchema#integer>')
  expect_equal(create_iri(as.Date("2024-10-30")), '"2024-10-30"^^<http://www.w3.org/2001/XMLSchema#date>')

  # prov_author
  expect_equal(prov_author(author_person),
               "<https://orcid.org/0000-0001-7513-6760> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> ."
  )
})

test_that("create_iri()", {
  author_person <- person(given = "Daniel", family = "Antal",
                          email = "daniel.antal@dataobservatory.eu",
                          role = c("aut", "cre"),
                          comment = c(ORCID = "0000-0001-7513-6760"))
  expect_error(create_iri(list(a=1:2)))
  #expect_output(print(create_iri(as.POSIXct(10000, origin = "2024-01-01", tz="UTC"))), "2024-01-01T03:46:40Z")
  #expect_output(print(create_iri(as.POSIXct(10000, origin = "2024-01-01", tz="UTC"))), "\\^\\^<xs:dateTime>")
  expect_equal(create_iri(author_person), "<https://orcid.org/0000-0001-7513-6760>")
  jane_doe <- person(given="Jane", family="Doe", role = "aut", email = "example@example.com")
  expect_equal(create_iri(x=jane_doe), "\"Jane Doe [aut]\"^^<http://www.w3.org/2001/XMLSchema#string>")
  joe_doe <- person(given="Joe", family="Doe", role = "aut", email = "example@example.com",
                     comment = c(Wikidata="https://www.wikidata.org/wiki/Q000"))
  expect_equal(create_iri(x=joe_doe), "<https://www.wikidata.org/wiki/Q000>")
  joe_doe <- person(given="Joe", family="Doe", role = "aut", email = "example@example.com",
                    comment = c(ISNI="1234"))
  expect_equal(create_iri(x=joe_doe), "<https://isni.org/isni/1234>")
  viaf_doe <- person(given="Joe", family="Doe", role = "aut", email = "example@example.com",
                    comment = c(VIAF="1234"))
  expect_equal(create_iri(x=viaf_doe), "<https://viaf.org/viaf/1234>")
  expect_equal(create_iri(x=2), "\"2\"^^<http://www.w3.org/2001/XMLSchema#double>")
  expect_true(grepl('http://www.w3.org/2001/XMLSchema#date>', create_iri(Sys.Date()) ))
})

test_that("get_person_iri() works", {
  author_person <- person(given = "Daniel", family = "Antal",
                          email = "daniel.antal@dataobservatory.eu",
                          role = c("aut", "cre"),
                          comment = c(ORCID = "0000-0001-7513-6760")
  )
  expect_equal(get_person_iri(author_person), "https://orcid.org/0000-0001-7513-6760")
  expect_equal(get_person_iri(person("Jane Doe")), NULL)
  expect_equal(get_person_iri(p=person(given="Daniel", family="Antal",
                                       role = "cre", comment=c(ORCID = "0000-0001-7513-6760"))),
               "https://orcid.org/0000-0001-7513-6760"
  )
  expect_equal(get_person_iri(p=
    person(given="Edgar", family="Anderson",
           role = "cre", comment=c(VIAF="http://viaf.org/viaf/6440526"))
    ),
           c(VIAF = "http://viaf.org/viaf/6440526"))
  expect_equal(get_person_iri(p=
                                person(given="Taylor", family="Swift", role = "cre", comment=c(ISNI="https://isni.org/isni/0000000078519858"))
  ),
  c(ISNI = "https://isni.org/isni/0000000078519858"))
})

