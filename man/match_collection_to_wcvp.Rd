% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_to_wcvp.R
\name{match_collection_to_wcvp}
\alias{match_collection_to_wcvp}
\title{Match collection to World Checklist of Vascular Plants via taxonomic name}
\usage{
match_collection_to_wcvp(
  collection,
  wcvp,
  taxon_name_column = "TaxonName",
  taxon_name_full_column = NA,
  taxon_author_column = NA,
  typo_method = "All",
  do_add_split = TRUE,
  do_fix_hybrid = TRUE,
  do_rm_autonym = TRUE,
  do_convert_accepted = TRUE,
  matching_criterion = BGSmartR::additional_wcvp_matching,
  ...,
  enrich_taxon_name_column = "taxon_name",
  enrich_taxon_authors_column = "sanitise_author",
  enrich_display_in_message_column = "powo_id",
  enrich_plant_identifier_column = "plant_name_id"
)
}
\arguments{
\item{collection}{A data frame containing a collection.}

\item{wcvp}{World Checklist of Vascular Plants (WCVP) database, obtained using the function \code{\link[=import_wcvp_names]{import_wcvp_names()}}.}

\item{taxon_name_column}{The name of the column in the \code{collection} corresponding to taxonomic names.}

\item{taxon_name_full_column}{The name of the column in the \code{collection} corresponding to joined taxonomic names and authors.}

\item{taxon_author_column}{The name of the column in the \code{collection} corresponding to the authors of the taxonomic names.}

\item{typo_method}{Either \code{'All'}, \code{'Data frame only'},\code{'Data frame + common'}, detailing the level of typo finding required.}

\item{do_add_split}{Flag (TRUE/FALSE) for whether we search for missing f./var./subsp.}

\item{do_fix_hybrid}{Flag (TRUE/FALSE) for whether we search for hybrid issues.}

\item{do_rm_autonym}{Flag (TRUE/FALSE) for whether we try removing autonyms.}

\item{do_convert_accepted}{Flag for whether we convert to accepted names in wcvp}

\item{matching_criterion}{A function used to chose the best method from extracts of the \code{wcvp$wcvp_names}.}

\item{...}{Arguments (i.e., attributes) used in the matching algorithm (passed along to nested fuctions). Examples include, \code{enrich_display_in_message_column} and \code{enrich_plant_identifier_column}.}

\item{enrich_taxon_name_column}{The name of the column in the \code{iucnRedlist} corresponding to taxonomic names.Default value is \code{scientific_name}.}

\item{enrich_taxon_authors_column}{The name of the column in \code{enrich_database} that corresponds to the authors of taxonomic names. Default value is \code{taxon_authors_simp}.}

\item{enrich_display_in_message_column}{The name of the column in \code{iucnRedlist} that contains values to show in the matching messages. Default value is \code{taxonid}.}

\item{enrich_plant_identifier_column}{The name of the column in \code{iucnRedlist} that corresponds to record identifier. Default value is \code{taxonid}.}
}
\value{
A list of length seven containing:
\itemize{
\item \verb{$match} the index of the record in \code{wcvp$wcvp_names} which matches the record in the collection database.
\item \verb{$details_short} a simplified message detailing the match.
\item \verb{$match_taxon_name} a longer format message detailing the match.
\item \verb{$original_authors} The author/s (extracted) from the \code{collection} database.
\item \verb{$match_authors} The author/s of the matched record in \code{wcvp$wcvp_names}.
\item \verb{$author_check} Either \code{Identical}, \code{Partial} or \code{Different}  (\verb{No Match} if a match to wcvp cannot be found). A message informing the similarity of the collection's taxon authors and the authors found in \code{wcvp$wcvp_names}. Author similarity is found using the function  \code{\link[=author_check]{author_check()}}.
}
}
\description{
Match collection to World Checklist of Vascular Plants via taxonomic name
}
\details{
This function allows matching of a collection's database to World Checklist of Vascular Plants (WCVP) database. For details of how the matching algorithm works see \verb{Method of Matching taxonomic records} vignette (Matching.Rmd).
}
