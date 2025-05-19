#' Taxon name typos
#'
#' A data frame of common typos found in the taxon name and their fixes.
#'
#' @format ## `typo_list`
#' A data frame with 8,253 rows and 2 columns:
#' \describe{
#'   \item{Original}{Original Taxon name with typo}
#'   \item{Fixed}{Fixed Taxon name}
#' }
"typo_list"

#' wgsrpd3_level1_simp
#'
#' A data frame containing BRU level 1 areas and their simplified geometries using rmapshaper::ms_simplify.
#'
#' @format ## `wgsrpd3_level1_simp`
#' A data frame with 9 rows and 3 columns:
#' \describe{
#'   \item{code}{The code of the area}
#'   \item{name}{The name of the area}
#'   \item{geometry}{The geometry of the area}
#' }
"wgsrpd3_level1_simp"

#' wgsrpd3_level2_simp
#'
#' A data frame containing BRU level 2 areas and their simplified geometries using rmapshaper::ms_simplify.
#'
#' @format ## `wgsrpd3_level2_simp`
#' A data frame with 52 rows and 3 columns:
#' \describe{
#'   \item{code}{The code of the area}
#'   \item{name}{The name of the area}
#'   \item{geometry}{The geometry of the area}
#' }
"wgsrpd3_level2_simp"

#' wgsrpd3_level3_simp
#'
#' A data frame containing BRU level 3 areas and their simplified geometries using rmapshaper::ms_simplify.
#'
#' @format ## `wgsrpd3_level3_simp`
#' A data frame with 369 rows and 3 columns:
#' \describe{
#'   \item{code}{The code of the area}
#'   \item{name}{The name of the area}
#'   \item{geometry}{The geometry of the area}
#' }
"wgsrpd3_level3_simp"

#' Diversity_classification
#'
#' A list containing two dataframes:
#'  - Classification: containing how families link to higher order groups.
#'  - synomym_families: containing family names that are synomym and which to use within Classificiation.
"Diversity_classification"

#' Tribe information
#'
#' A data frame containing tribe information used for creating sunburst/treemap charts of taxonomic diversity. Source WFO.
"tribe_genus_simp"

#' Sub-family information
#'
#' A data frame containing sub-family information used for creating sunburst/treemap charts of taxonomic diversity. Source WFO.
"subfamily_genus_simp"

#' Sub-tribe information
#'
#' A data frame containing sub-tribe information used for creating sunburst/treemap charts of taxonomic diversity. Source WFO.
"subtribe_genus_simp"

#' Example collection data
#'
#' A data frame containing a fake collection to be used in examples
"collection_example"

#' Example enriched collection data
#'
#' A data frame containing a fake enriched collection to be used in examples, corresponds to the data in "collection_example"
"enriched_collection_example"
