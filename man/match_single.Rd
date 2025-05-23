% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matching_functions.R
\name{match_single}
\alias{match_single}
\alias{match_multiple}
\alias{match_all_issue}
\alias{match_typos}
\alias{no_match_cultivar_indet}
\alias{get_match_from_multiple}
\alias{check_taxon_typo}
\alias{shorten_message}
\alias{try_rm_autonym}
\alias{try_fix_infraspecific_level}
\alias{try_fix_hybrid}
\title{Matching functions}
\usage{
match_single(
  taxon_names,
  enrich_database,
  enrich_database_search_index,
  enrich_taxon_name_column = "taxon_name",
  enrich_display_in_message_column = "ID",
  match_column = NA,
  ...
)

match_multiple(
  taxon_names,
  taxon_authors,
  enrich_database,
  enrich_database_search_index,
  enrich_taxon_name_column = "taxon_name",
  enrich_display_in_message_column = "ID",
  enrich_plant_identifier_column = "ID",
  match_column = NA,
  ...,
  show_progress = TRUE
)

match_all_issue(
  taxon_names,
  taxon_authors = rep(NA, length(taxon_names)),
  enrich_database,
  matching_authors = BGSmartR::match_authors,
  matching_criterion = BGSmartR::additional_wcvp_matching,
  do_add_split = TRUE,
  do_fix_hybrid = TRUE,
  do_rm_autonym = TRUE,
  enrich_taxon_name_column = "taxon_name",
  enrich_taxon_authors_column = "taxon_authors_simp",
  enrich_plant_identifier_column = "ID",
  enrich_display_in_message_column = "ID",
  ...
)

match_typos(
  taxon_names,
  taxon_authors,
  enrich_database,
  enrich_taxon_name_column = "taxon_name",
  single_indices = NA,
  mult_indices = NA,
  typo_method = "Data frame only",
  do_match_multiple = TRUE,
  ...
)

no_match_cultivar_indet(taxon_names)

get_match_from_multiple(
  taxon_name_and_author,
  enrich_database_mult,
  matching_authors = BGSmartR::match_authors,
  matching_criterion = BGSmartR::no_additional_matching,
  enrich_plant_identifier_column = "plant_name_id",
  enrich_taxon_name_column = "taxon_name",
  enrich_taxon_authors_column = "taxon_authors_simp",
  enrich_taxon_author_words_column = "author_parts",
  ...
)

check_taxon_typo(
  taxon_name,
  enrich_database = NA,
  enrich_taxon_name_column = "taxon_name",
  typo_df = BGSmartR::typo_list,
  typo_method = "Data frame only",
  ...
)

shorten_message(messages)

try_rm_autonym(
  taxon_names,
  enrich_database_taxon_names,
  console_message = TRUE,
  ...
)

try_fix_infraspecific_level(
  taxon_names,
  enrich_database_taxon_names,
  try_hybrid = TRUE,
  console_message = TRUE,
  ...
)

try_fix_hybrid(
  taxon_names,
  enrich_database_taxon_names,
  try_hybrid = TRUE,
  console_message = TRUE,
  ...
)
}
\arguments{
\item{taxon_names}{Vector of taxonomic names.}

\item{enrich_database}{A data frame of enriching information we want to match \code{taxon_names} to.}

\item{enrich_database_search_index}{A vector of indices of \code{enrich_database} that are desired to be matched to.}

\item{enrich_taxon_name_column}{The name of the column in \code{enrich_database} that corresponds to taxonomic names. Default value is \code{taxon_names}.}

\item{enrich_display_in_message_column}{The name of the column in \code{enrich_database} that contains values to show in the matching messages. Default value is \code{powo_id} (wcvp identifier).}

\item{match_column}{either \code{NA} or the name of the column in \code{enrich_database}. The default value if \code{NA} which means the values of the match are the indices of the matched records in the enrich database. If instead a single column of \code{enrich_database} is desired to be the result of the match the name of the column needs to be provided.}

\item{...}{Arguments (i.e., attributes) used in the matching algorithm (passed along to nested fuctions). Examples include \code{enrich_taxon_authors_column}, \code{enrich_display_in_message_column} and \code{enrich_plant_identifier_column}.}

\item{taxon_authors}{A vector of full taxon names (corresponding to \code{taxon_names})}

\item{enrich_plant_identifier_column}{The name of the column in \code{enrich_database} that corresponds to record identifier. Default value is \code{plant_name_id}.}

\item{show_progress}{Flag (TRUE/FALSE) for whether we show progress bar.}

\item{matching_authors}{The function used to find the best match using the author of taxonomic names. By default the function \code{BGSmartR::match_authors()} is used.}

\item{matching_criterion}{The function used to find the best match when we have 'non-unique' taxonomic names. By default the function \code{BGSmartR::get_match_from_multiple()} is used.}

\item{do_add_split}{Flag (TRUE/FALSE) for whether we search for missing f./var./subsp.}

\item{do_fix_hybrid}{Flag (TRUE/FALSE) for whether we search for hybrid issues.}

\item{do_rm_autonym}{Flag (TRUE/FALSE) for whether we try removing autonyms.}

\item{enrich_taxon_authors_column}{The name of the column in \code{enrich_database} that corresponds to the authors of taxonomic names. Default value is \code{taxon_authors_simp}.}

\item{single_indices}{A vector of indices of \code{enrich_database} that correspond to the records that have 'unique' taxonomic names.}

\item{mult_indices}{A vector of indices of \code{enrich_database} that correspond to the records that have 'non-unique' taxonomic names.}

\item{typo_method}{Either \code{'All'}, \code{'Data frame only'},\code{'Data frame + common'}, detailing the level of typo finding required.}

\item{do_match_multiple}{Flag (TRUE/FALSE) for whether we attempt matching those found to have multiple taxonomic names in the enrich database..}

\item{taxon_name_and_author}{the pair of taxonomic name and combined taxonomic name and author}

\item{enrich_database_mult}{\code{enrich_database} restricted to the rows that correspond to 'non-unique' taxonomic names.}

\item{enrich_taxon_author_words_column}{The name of the column in \code{enrich_database} that corresponds to the words contained in the authors of taxonomic names. Default value is \code{author_parts}.}

\item{taxon_name}{A single taxonomic name.}

\item{typo_df}{A data frame where the first column is a taxonomic name with a typo and the second column is the corrected taxonomic name. By default \code{BGSmartR::typo_list} is used.}

\item{messages}{messages detailing how a match is obtained.}

\item{enrich_database_taxon_names}{The taxon names taken from \code{enrich_database}.}

\item{console_message}{Flag (TRUE/FALSE) detailing whether to show messages in the console.}

\item{try_hybrid}{Flag (TRUE/FALSE) for whether hybrid fixes are attempted.}
}
\description{
Functions used to match taxonomic names from a collection to exterior databases (POWO's WCVP, IUCN Redlist)
}
\details{
Below we outline the uses of each function. For further details and examples on matching functions please see the \code{Matching.Rmd} vignette.

Each of the matching functions generally return the index of the matching record in \code{enrich_database} and a message detailing how the match was obtained. These function can be used as building blocks to build a custom taxonomic name matching algorithm.
\itemize{
\item \code{match_single()} matches \code{taxon_names} to \code{enrich_database} taking only the first match. \code{enrich_database_search_index} should be used to restrict the enrich database to only 'unique' taxonomic names (i.e taxonomic names that correspond to a single record in the enrich database). For 'non-unique' taxonomic names \code{match_multiple()} should be used.
\item \code{match_multiple()} matches \code{taxon_names} to \code{enrich_database} for entries in enrich database that have \code{non-unique} taxonomic names. For 'unique' taxonomic names \code{match_single()} should be used. For \code{non-unique} taxonomic names we first use taxonomic author matching to decide which record to use. This matching is performed to each taxonomic name and author using the function \code{get_match_from_multiple()}. \code{get_match_from_multiple()} further depends on a  matching criteria function which can be added using the input \code{matching_criterion} (passed via \code{...}). By default this is set to \code{\link[=additional_wcvp_matching]{additional_wcvp_matching()}}, which uses accepted_plant_name_id and taxon_status to chose the best match (in WCVP).
\item \code{match_all_issue()} attempts to fix hybridisation, change infraspecific levels or remove autonyms to find matches to an enriched database. This function depends on the functions:
\itemize{
\item \code{try_rm_autonym()} attempts to find taxonomic names in \code{enrich_database} by removing autonyms.
\item \code{try_fix_infraspecific_level()} attempts to find taxonomic names in \code{enrich_database} by adding/changing/removing infraspecific levels (var., f., etc).
\item \code{try_fix_hybrid()}  attempts to find taxonomic names in \code{enrich_database} by adding/changing/removing hybrid markers (+ or x).
}
\item \code{match_typos()} attempts to find matches by searching for typos in the taxonomic name. This depends on the function:
\itemize{
\item \code{check_taxon_typo()} to check a single taxonomic name for typos found either in a typo list or the enriched database.
}
\item \code{no_match_cultivar_indet()} searches for cultivars and indeterminates and sets their match to \code{-1} indicating no match.
\item \code{shorten_message()} compresses matching message (details of how a match is found) into an easy to read format.
}
}
