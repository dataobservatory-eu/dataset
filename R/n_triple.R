#' @title Create N-Triples
#' @description Create triple statements to annotate your dataset with standard,
#' interoperable metadata.
#' @details N-Triples is an easy to parse line-based subset of Turtle to serialize
#' RDF.
#' @param triples Concatenated N-Triples created with \code{\link{n_triple}}.
#' @return A character vector containing unique N-Triple strings.
#' @examples
#' triple_1 <- n_triple("http://example.org/show/218",
#'                       "http://www.w3.org/2000/01/rdf-schema#label",
#'                       "That Seventies Show")
#' triple_2 <- n_triple("http://example.org/show/218",
#'                      "http://example.org/show/localName",
#'                      '"Cette Série des Années Septante"@fr-be')
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
#' @examples
#' s <- "http://example.org/show/218"
#' p <- "http://www.w3.org/2000/01/rdf-schema#label"
#' o <- "That Seventies Show"
#' n_triple(s, p, o)
#' @export
n_triple <- function(s,p,o) {

  s <- create_iri(s)
  p <- create_iri(p)
  o <- create_iri(o)

  sprintf('%s %s %s .', s, p, o)
}

#' @keywords internal
create_iri <- function(x) {

  if ( any(c("list", "data.frame", "tbl", "data.table") %in% class(x)) ) {
    stop ("Error: create_iri(x) must be any of an URI, string, integer, double, Date, or dateTime.")
  }

  double_string <- '^^<http://www.w3.org/2001/XMLSchema#double>'
  integer_string <- '^^<http://www.w3.org/2001/XMLSchema#integer>'
  character_string <- '^^<http://www.w3.org/2001/XMLSchema#string>'
  date_string <- '^^<http://www.w3.org/2001/XMLSchema#date>'

  if(is.integer(x)) {
    sprintf('"%s"%s', as.character(x), integer_string)
  } else if( is.character(x) & substr(x, 1, 5) %in% c("http:", "https")) {
    sprintf('<%s>', as.character(x))
  } else if ( grepl("^_\\:", x)) {
    sprintf('"%s"', x)
  } else if ( grepl("@", x)) {
    sprintf('"%s"', x)
  } else if ( inherits(x, "Date") ) {
    sprintf('"%s"%s', as.character(x), date_string)
  } else if(is.numeric(x)) {
    sprintf('"%s"%s', as.character(x), double_string)
  } else if (x=="a") {
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'
  } else if (is.character(x)) {
    sprintf('"%s"%s', as.character(x), character_string)
  }
}


#' @keywords internal
prov_author <- function(author_person) {

  if (inherits(author_person, "person")) {
    print_name <- "_:"
    if (!is.null(author_person$family)) print_name <- paste0(print_name, tolower(author_person$family))
    if (!is.null(author_person$given))  print_name <- paste0(print_name, tolower(author_person$given))
    orcid <- get_orcid(author_person)

  } else if (is.character(attr(author_person, "person"))) {
    print_name <- paste0(attr(author_person, "person"), ": ")
  } else { print_name = ""}

  if(!is.null(orcid)) {
    triple_1 <- n_triple(orcid, "a", "http://www.w3.org/ns/prov#Agent")
  }

  n_triple(print_name, "a", "http://www.w3.org/ns/prov#Agent")
}


#' @keywords internal
get_orcid <- function(p) {
  if (!is.null(p$comment)) {
    if ( any(c("ORCID", "ORCiD", "orcid") %in% names(p$comment))) {
      comment_n <- which(names(p$comment) %in% c("ORCID", "ORCiD", "orcid"))[1]
      p$comment[comment_n]
    }
  } else NULL
}



