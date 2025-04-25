test_that("n_triples()", {
  triple_1 <- n_triple("http://example.org/show/218", "http://www.w3.org/2000/01/rdf-schema#label", "That Seventies Show")
  triple_2 <- n_triple("http://example.org/show/218", "http://example.org/show/localName", '"Cette Série des Années Septante"@fr-be')
  author_person <- person(given = "Daniel", family = "Antal", comment = c(ORCID = "https://orcid.org/0000-0001-7513-6760"))
  expect_equal(triple_1, "<http://example.org/show/218> <http://www.w3.org/2000/01/rdf-schema#label> \"That Seventies Show\"^^<http://www.w3.org/2001/XMLSchema#string> .")
  expect_equal(
    n_triple(
      s = "http://example.com/creation",
      p = "http://www.w3.org/ns/prov#generatedAtTime",
      o = as.POSIXct(10000, origin = "2024-01-01", tz = "UTC")
    ),
    "<http://example.com/creation> <http://www.w3.org/ns/prov#generatedAtTime> \"2024-01-01T02:46:40Z\"^^<http://www.w3.org/2001/XMLSchema#dateTime> ."
  )
  expect_equal(length(n_triples(c(triple_1, triple_2, triple_1))), 2)
  expect_equal(length(n_triples(c(triple_1, triple_2))), 2)
  expect_equal(
    n_triple("https://orcid.org/0000-0001-7513-6760", "a", "http://www.w3.org/ns/prov#Agent"),
    "<https://orcid.org/0000-0001-7513-6760> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> ."
  )
  expect_equal(create_iri(23), '\"23\"^^<http://www.w3.org/2001/XMLSchema#double>')
  expect_equal(create_iri("23"), '\"23\"^^<http://www.w3.org/2001/XMLSchema#string>')
  expect_equal(create_iri(as.integer(23)), '\"23\"^^<http://www.w3.org/2001/XMLSchema#integer>')
  expect_equal(create_iri(as.Date("2024-10-30")), '\"2024-10-30\"^^<http://www.w3.org/2001/XMLSchema#date>')
  expect_equal(prov_author(author_person), "<https://orcid.org/0000-0001-7513-6760> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> .")
})

test_that("n_triple() handles POSIXct datetime correctly", {
  creation_time <- as.POSIXct(10000, origin = "2024-01-01", tz = "UTC")
  formatted_time <- format(creation_time, "%Y-%m-%dT%H:%M:%SZ")

  expect_equal(
    n_triple(
      s = "http://example.com/creation",
      p = "http://www.w3.org/ns/prov#generatedAtTime",
      o = creation_time
    ),
    paste0(
      "<http://example.com/creation> ",
      "<http://www.w3.org/ns/prov#generatedAtTime> ",
      "\"", formatted_time, "\"^^<http://www.w3.org/2001/XMLSchema#dateTime> ."
    )
  )
})

test_that("prov_author()", {
  expect_equal(
    prov_author(person(
      given = "Daniel", family = "Antal",
      email = "daniel.antal@dataobservatory.eu",
      role = c("aut", "cre"),
      comment = c(ORCID = "0000-0001-7513-6760")
    )),
    "<https://orcid.org/0000-0001-7513-6760> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/prov#Agent> ."
  )
})

test_that("create_iri()", {
  author_person <- person(
    given = "Daniel", family = "Antal",
    email = "daniel.antal@dataobservatory.eu",
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0001-7513-6760")
  )
  expect_error(create_iri(list(a = 1:2)))
  expect_true(grepl("2024-01-01T02:46:40Z", create_iri(as.POSIXct(10000, origin = "2024-01-01", tz = "UTC"))))
  expect_true(grepl("http://www.w3.org/2001/XMLSchema#dateTime", create_iri(as.POSIXct(10000, origin = "2024-01-01", tz = "UTC"))))
  expect_equal(create_iri(author_person), "<https://orcid.org/0000-0001-7513-6760>")
  jane_doe <- person(given = "Jane", family = "Doe", role = "aut", email = "example@example.com")
  expect_equal(create_iri(x = jane_doe), "\"Jane Doe [aut]\"^^<http://www.w3.org/2001/XMLSchema#string>")
  joe_doe <- person(
    given = "Joe", family = "Doe", role = "aut", email = "example@example.com",
    comment = c(Wikidata = "https://www.wikidata.org/wiki/Q000")
  )
  expect_equal(create_iri(x = joe_doe), "<https://www.wikidata.org/wiki/Q000>")
  joe_doe <- person(
    given = "Joe", family = "Doe", role = "aut", email = "example@example.com",
    comment = c(ISNI = "1234")
  )
  expect_equal(create_iri(x = joe_doe), "<https://isni.org/isni/1234>")
  viaf_doe <- person(
    given = "Joe", family = "Doe", role = "aut", email = "example@example.com",
    comment = c(VIAF = "1234")
  )
  expect_equal(create_iri(x = viaf_doe), "<https://viaf.org/viaf/1234>")
  expect_equal(create_iri(x = 2), "\"2\"^^<http://www.w3.org/2001/XMLSchema#double>")
  expect_true(grepl("http://www.w3.org/2001/XMLSchema#date>", create_iri(Sys.Date())))
})

test_that("get_person_iri() works", {
  author_person <- person(
    given = "Daniel", family = "Antal",
    email = "daniel.antal@dataobservatory.eu",
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0001-7513-6760")
  )
  expect_equal(get_person_iri(author_person), "https://orcid.org/0000-0001-7513-6760")
  expect_equal(get_person_iri(person("Jane Doe")), NULL)
  expect_equal(
    get_person_iri(p = person(
      given = "Daniel", family = "Antal",
      role = "cre", comment = c(ORCID = "0000-0001-7513-6760")
    )),
    "https://orcid.org/0000-0001-7513-6760"
  )
  expect_equal(
    get_person_iri(
      p =
        person(
          given = "Edgar", family = "Anderson",
          role = "cre", comment = c(VIAF = "http://viaf.org/viaf/6440526")
        )
    ),
    c(VIAF = "http://viaf.org/viaf/6440526")
  )
  expect_equal(
    get_person_iri(
      p =
        person(given = "Taylor", family = "Swift", role = "cre", comment = c(ISNI = "https://isni.org/isni/0000000078519858"))
    ),
    c(ISNI = "https://isni.org/isni/0000000078519858")
  )
})
