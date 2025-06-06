#' @title Create N-Triples
#' @description Create triple statements to annotate your dataset with standard,
#' interoperable metadata.
#' @details N-Triples is an easy to parse line-based subset of Turtle to serialize
#' RDF. See
#' \href{https://www.w3.org/TR/rdf12-n-triples/}{RDF 1.2 N-Triples. A line-based syntax for an RDF graph}.
#' @param triples Concatenated N-Triples created with \code{\link{n_triple}}.
#' @return A character vector containing unique N-Triple strings.
#' @examples
#' triple_1 <- n_triple(
#'   "http://example.org/show/218",
#'   "http://www.w3.org/2000/01/rdf-schema#label",
#'   "That Seventies Show"
#' )
#' triple_2 <- n_triple(
#'   "http://example.org/show/218",
#'   "http://example.org/show/localName",
#'   '"Cette Série des Années Septante"@fr-be'
#' )
#' n_triples(c(triple_1, triple_2, triple_1))
#' @export

n_triples <- function(triples) {
  unique(triples)
}

#' @title Create an N-Triple
#' @description Create a single N-Triple triple.
#' @details N-Triples is an easy to parse line-based subset of Turtle to serialize
#' RDF. An N-Triple triple is a sequence of RDF terms representing the subject,
#'  predicate and object of an RDF Triple. Use \code{\link{n_triples}} to serialize
#'  multiple statements.
#' @source \href{https://www.w3.org/TR/n-triples/}{RDF 1.1 N-Triples}
#' @param s The subject of a triplet.
#' @param p The predicate of a triplet.
#' @param o The object of a triplet.
#' @return A character vector containing one N-Triple string.
#' @examples
#' s <- "http://example.org/show/218"
#' p <- "http://www.w3.org/2000/01/rdf-schema#label"
#' o <- "That Seventies Show"
#' n_triple(s, p, o)
#' @export

n_triple <- function(s, p, o) {
  s <- create_iri(s)
  p <- create_iri(p)
  o <- create_iri(o)

  sprintf("%s %s %s .", s, p, o)
}

#' @keywords internal
create_iri <- function(x) {
  if (any(c("list", "data.frame", "tbl", "data.table") %in% class(x))) {
    stop("Error: create_iri(x) must be a URI, string, integer, double, Date, dateTime, or person.")
  }

  double_string <- "^^<http://www.w3.org/2001/XMLSchema#double>"
  integer_string <- "^^<http://www.w3.org/2001/XMLSchema#integer>"
  character_string <- "^^<http://www.w3.org/2001/XMLSchema#string>"
  date_string <- "^^<http://www.w3.org/2001/XMLSchema#date>"
  datetime_string <- "^^<http://www.w3.org/2001/XMLSchema#dateTime>"

  # Person object handling
  if (inherits(x, "person")) {
    if ("isni" %in% tolower(names(x$comment))) {
      x <- paste0("https://isni.org/isni/", x$comment[which(tolower(names(x$comment)) == "isni")])
    } else if ("orcid" %in% tolower(names(x$comment))) {
      x <- paste0("https://orcid.org/", x$comment[which(tolower(names(x$comment)) == "orcid")])
    } else if ("viaf" %in% tolower(names(x$comment))) {
      x <- paste0("https://viaf.org/viaf/", x$comment[which(tolower(names(x$comment)) == "viaf")])
    } else if ("wikidata" %in% tolower(names(x$comment))) {
      qid <- x$comment[which(tolower(names(x$comment)) == "wikidata")]
      qid <- gsub("https://www.wikidata.org/wiki/", "", qid)
      x <- paste0("https://www.wikidata.org/wiki/", qid)
    } else {
      # No external ID found; format manually
      full_name <- c(x$given, x$family)
      full_name <- full_name[!is.null(full_name) & nzchar(full_name)]
      name_str <- paste(full_name, collapse = " ")

      roles <- if (!is.null(x$role) && length(x$role) > 0 && any(nzchar(x$role))) {
        paste0(" [", paste(x$role, collapse = ", "), "]")
      } else {
        ""
      }

      x <- paste0(name_str, roles)
    }
  }

  # Branching on type
  if (is.integer(x)) {
    sprintf('"%s"%s', as.character(x), integer_string)
  } else if (inherits(x, "POSIXct")) {
    sprintf('"%s"%s', format(x, "%Y-%m-%dT%H:%M:%SZ"), datetime_string)
  } else if (is.character(x) && (substr(x, 1, 5) %in% c("http:", "https"))) {
    sprintf("<%s>", as.character(x))
  } else if (grepl("^_\\:", x)) {
    sprintf('"%s"', x)
  } else if (grepl("@", x)) {
    sprintf('"%s"', x)
  } else if (inherits(x, "Date")) {
    sprintf('"%s"%s', as.character(x), date_string)
  } else if (is.numeric(x)) {
    sprintf('"%s"%s', as.character(x), double_string)
  } else if (x == "a") {
    "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"
  } else if (is.character(x)) {
    x <- gsub("DCMITYPE\\:", "http://purl.org/dc/dcmitype/", x)
    sprintf('"%s"%s', as.character(x), character_string)
  }
}


#' @keywords internal
prov_author <- function(author_person) {
  if (inherits(author_person, "person")) {
    print_name <- "_:"
    if (!is.null(author_person$family)) print_name <- paste0(print_name, tolower(author_person$family))
    if (!is.null(author_person$given)) print_name <- paste0(print_name, tolower(author_person$given))
    person_iri <- get_person_iri(author_person)
  } else if (is.character(attr(author_person, "person"))) {
    print_name <- paste0(attr(author_person, "person"), ": ")
  } else {
    print_name <- ""
  }

  if (!is.null(person_iri)) {
    n_triple(person_iri, "a", "http://www.w3.org/ns/prov#Agent")
  } else {
    n_triple(print_name, "a", "http://www.w3.org/ns/prov#Agent")
  }
}


#' @keywords internal
get_person_iri <- function(p) {
  assertthat::assert_that(inherits(p, "person"),
    msg = "Error: get_person_iri(p): p is not a utils::person object."
  )

  if (!is.null(p$comment)) {
    if (any(c("ORCID", "ORCiD", "orcid") %in% names(p$comment))) {
      comment_n <- which(names(p$comment) %in% c("ORCID", "ORCiD", "orcid"))[1]
      orcid <- p$comment[comment_n]
      if (!grepl("https://orcid.org/", orcid)) orcid <- paste0("https://orcid.org/", orcid)
      orcid
    } else if (any("isni" %in% tolower(names(p$comment)))) {
      comment_n <- which(tolower(names(p$comment)) == "isni")[1]
      isni <- p$comment[comment_n]
      if (!grepl("https://isni.org/isni/", isni)) isni <- paste0("https://isni.org/isni/", isni)
      isni
    } else if (any("viaf" %in% tolower(names(p$comment)))) {
      comment_n <- which(tolower(names(p$comment)) == "viaf")[1]
      viaf <- p$comment[comment_n]
      if (!grepl("http://viaf.org/viaf/", viaf)) viaf <- paste0("http://viaf.org/viaf/", viaf)
      viaf
    }
  } else {
    NULL
  }
}
