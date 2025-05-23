% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_to_iucnRedlist.R
\name{match_collection_to_iucnRedlist}
\alias{match_collection_to_iucnRedlist}
\title{Match collection to IUCN Red List of Threatened Species via taxonomic name}
\usage{
match_collection_to_iucnRedlist(
  collection,
  iucnRedlist,
  taxon_name_column = "TaxonName",
  taxon_name_full_column = NA,
  taxon_author_column = NA,
  typo_method = "All",
  do_add_split = TRUE,
  do_fix_hybrid = TRUE,
  do_rm_autonym = TRUE,
  ...,
  enrich_taxon_name_column = "scientific_name",
  enrich_taxon_authors_column = "sanitise_author",
  enrich_display_in_message_column = "taxonid",
  enrich_plant_identifier_column = "taxonid",
  matching_criterion = BGSmartR::no_additional_matching,
  try_hybrid = FALSE
)
}
\arguments{
\item{collection}{A data frame containing a collection.}

\item{iucnRedlist}{IUCN Red List of Threatened Species database, obtained using the function XXXX.}

\item{taxon_name_column}{The name of the column in the \code{collection} corresponding to taxonomic names.}

\item{taxon_name_full_column}{The name of the column in the \code{collection} corresponding to joined taxonomic names and authors.}

\item{taxon_author_column}{The name of the column in the \code{collection} corresponding to the authors of the taxonomic names.}

\item{typo_method}{Either \code{'All'}, \code{'Data frame only'},\code{'Data frame + common'}, detailing the level of typo finding required.}

\item{do_add_split}{Flag (TRUE/FALSE) for whether we search for missing f./var./subsp.}

\item{do_fix_hybrid}{Flag (TRUE/FALSE) for whether we search for hybrid issues.}

\item{do_rm_autonym}{Flag (TRUE/FALSE) for whether we try removing autonyms.}

\item{...}{Arguments (i.e., attributes) used in the matching algorithm (passed along to nested functions). Examples include \code{enrich_taxon_authors_column}, \code{enrich_display_in_message_column} and \code{enrich_plant_identifier_column}.}

\item{enrich_taxon_name_column}{The name of the column in the \code{iucnRedlist} corresponding to taxonomic names.Default value is \code{scientific_name}.}

\item{enrich_taxon_authors_column}{The name of the column in \code{enrich_database} that corresponds to the authors of taxonomic names. Default value is \code{sanitise_author}.}

\item{enrich_display_in_message_column}{The name of the column in \code{iucnRedlist} that contains values to show in the matching messages. Default value is \code{taxonid}.}

\item{enrich_plant_identifier_column}{The name of the column in \code{iucnRedlist} that corresponds to record identifier. Default value is \code{taxonid}.}

\item{matching_criterion}{A function used to chose the best method from extracts of the \code{iucnRedlist}.}

\item{try_hybrid}{Flag (TRUE/FALSE) whether we want to look at hybrid fixes across all fixing methods.}
}
\value{
A list of length seven containing:
\itemize{
\item \verb{$match} the index of the record in \code{iucnRedlist} which matches the record in the collection database.
\item \verb{$details_short} a simplified message detailing the match.
\item \verb{$match_taxon_name} a longer format message detailing the match.
\item \verb{$original_authors} The author/s (extracted) from the \code{collection} database.
\item \verb{$match_authors} The author/s of the matched record in \code{iucnRedlist}.
\item \verb{$author_check} Either \code{Identical}, \code{Partial} or \code{Different}  (\verb{No Match} if a match to iucnRedlist cannot be found). A message informing the similarity of the collection's taxon authors and the authors found in \code{iucnRedlist}. Author similarity is found using the function  \code{\link[=author_check]{author_check()}}.
}

match = taxon_match_full,
}
\description{
Match collection to IUCN Red List of Threatened Species via taxonomic name
}
\details{
This function allows matching of a collection's database to IUCN Red List of Threatened Species database. This function relies of matching functions found that are documented in  \code{\link[=match_single]{match_single()}}, and is broadly similar to the function \code{\link[=match_collection_to_wcvp]{match_collection_to_wcvp()}}.

Note that by default the matching functions use column names from (WCVP) therefore these will often require changing prior to matching unless you change the column names in iucnRedlist to concure with wcvp column names.

Within the algorithm there exists methods to improve the matching such as trying to change infraspecific levels (e.g var. to subsp.) or adding hybridisation. These methods can be turned on and off using \code{do_add_split}, \code{do_fix_hybrid}, \code{do_rm_autonym} and \code{typo_method}.
}
