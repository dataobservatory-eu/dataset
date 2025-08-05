#' @rdname datacite
#' @param x A dataset object created with \code{dataset::\link{dataset}}.
#' @param type A DataCite 4.4  metadata can be returned as a
#' \code{type="list"}, a \code{type="dataset_df"}, a \code{type="bibentry"}
#' (default) or  \code{type="ntriples"}.
#' @param ... Optional parameters to add to a \code{datacite} object.
#'   \code{author=person("Jane", "Doe")} adds an author to the citation object
#'   if \code{type="dataset"}. as_datacite(orange_df, type="list")
#' @return \code{as_datacite(x, type)} returns the DataCite bibliographical
#'   metadata of x either as a list, a bibentry object, an N-Triples text
#'   serialisation or a dataset_df object.
#' @export
as_datacite <- function(x, type = "bibentry", ...) {
  citation_author <- person(NULL, NULL)

  is_person <- function(p) ifelse(inherits(p, "person"), TRUE, FALSE)

  arguments <- list(...)
  if (!is.null(arguments$author)) {
    if (is_person(arguments$author)) {
      citation_author <- arguments$author
    } else {
      stop("as_datacite(x, ..., author = ): author must be created with utils::person().")
    }
  }

  if (!type %in% c("bibentry", "list", "dataset_df", "ntriples")) {
    warning_message <- "as_datacite(ds, type=...) type cannot be "
    warning(warning_message, type, ". Reverting to 'bibentry'.")
    type <- "bibentry"
  }

  ds_bibentry <- get_bibentry(x)
  Title <- ds_bibentry$title
  Creator <- ds_bibentry$author
  Publisher <- ifelse(is.null(ds_bibentry$publisher), ":unas", as.character(ds_bibentry$publisher))
  Identifier <- ifelse(is.null(ds_bibentry$identifier), ":tba", as.character(ds_bibentry$identifier))
  Version <- ifelse(is.null(ds_bibentry$version), ":unas", as.character(ds_bibentry$version))
  Description <- ifelse(is.null(ds_bibentry$description), ":unas", as.character(ds_bibentry$description))
  Language <- ifelse(is.null(ds_bibentry$language), ":unas", as.character(ds_bibentry$language))
  Date <- ifelse(is.null(ds_bibentry$Date), ":tba", as.character(ds_bibentry$Date))
  DateList <- ifelse(is.null(ds_bibentry$DateList), ":tba", as.character(ds_bibentry$DateList))
  PublicationYear <- ifelse(is.null(ds_bibentry$year), ":unas", as.character(ds_bibentry$year))
  Format <- ifelse(is.null(ds_bibentry$format), ":tba", as.character(ds_bibentry$format))
  AlternateIdentifier <- ifelse(is.null(ds_bibentry$alternateidentifier), ":unas", ds_bibentry$alternateidentifier)
  RelatedIdentifier <- ifelse(is.null(ds_bibentry$relatedidentifier), ":unas", ds_bibentry$relatedidentifier)
  Rights <- ifelse(is.null(ds_bibentry$rights), ":tba", as.character(ds_bibentry$rights))
  Geolocation <- ifelse(is.null(ds_bibentry$geolocation), ":unas", as.character(ds_bibentry$geolocation))
  FundingReference <- ifelse(is.null(ds_bibentry$fundingreference), ":unas", as.character(ds_bibentry$fundingreference))
  Contributor <- ifelse(is.null(ds_bibentry$contributor), "", as.character(ds_bibentry$contributor))
  Subject <- ifelse(is.null(subject(x)), new_Subject(":tba"), subject(x))

  if (type == "bibentry") {
    new_datacite(
      Title = Title,
      Creator = Creator,
      Identifier = Identifier,
      Publisher = Publisher,
      PublicationYear = PublicationYear,
      Subject = Subject,
      Type = "Dataset",
      Contributor = Contributor,
      Date = Date,
      DateList = DateList,
      Language = Language,
      AlternateIdentifier = AlternateIdentifier,
      RelatedIdentifier = RelatedIdentifier,
      Format = Format,
      Version = Version,
      Rights = Rights,
      Description = Description,
      Geolocation = Geolocation,
      FundingReference = FundingReference
    )
  } else if (type == "list") {
    list(
      Title = Title,
      Creator = Creator,
      Identifier = Identifier,
      Publisher = Publisher,
      PublicationYear = PublicationYear,
      Subject = Subject,
      Type = "Dataset",
      Contributor = Contributor,
      Date = Date,
      DateList = DateList,
      Language = Language,
      AlternateIdentifier = AlternateIdentifier,
      RelatedIdentifier = RelatedIdentifier,
      Format = Format,
      Version = Version,
      Rights = Rights,
      Description = Description,
      Geolocation = Geolocation,
      FundingReference = FundingReference
    )
  } else if (type == "dataset_df") {
    dataset_df(
      data.frame(
        Title = Title,
        Creator = paste(
          vapply(
            Creator,
            function(p) p$family, character(1)
          ),
          collapse = "; "
        ),
        Identifier = Identifier,
        Publisher = Publisher,
        PublicationYear = PublicationYear,
        Subject = ifelse(is.null(Subject), "",
          as.character(Subject)
        ),
        Type = "Dataset",
        Contributor = ifelse(is.null(Contributor),
          ":unas", as.character(Contributor)
        ),
        Date = Date,
        DateList = DateList,
        Language = Language,
        AlternateIdentifier = AlternateIdentifier,
        RelatedIdentifier = RelatedIdentifier,
        Format = Format,
        Version = Version,
        Rights = Rights,
        Description = Description,
        Geolocation = Geolocation,
        FundingReference = FundingReference
      ),
      dataset_bibentry = datacite(
        Title = paste0("The DataCite metadata of `", Title, "`"),
        Creator = Creator,
        Identifier = Identifier,
        Date = Date
      )
    )
  } else if (type == "ntriples") {
    dc_list <- list(
      title = Title,
      creator = paste(vapply(Creator, function(p) p$family, character(1)), collapse = "; "),
      identifier = Identifier,
      publisher = Publisher,
      publicationyear = PublicationYear,
      subject = ifelse(is.null(Subject), "", as.character(Subject)),
      type = "Dataset",
      contributor = Contributor,
      date = Date,
      datelist = DateList,
      language = Language,
      alternateidentifier = AlternateIdentifier,
      relatedidentifier = RelatedIdentifier,
      format = Format,
      version = Version,
      rights = Rights,
      description = Description,
      geolocation = Geolocation,
      fundingreference = FundingReference
    )

    dataset_id <- if (Identifier == ":tba") {
      "http://example.com/dataset_tba/"
    } else {
      Identifier
    }

    datacite_to_triples(dc_list, dataset_id = dataset_id)
  }
}
