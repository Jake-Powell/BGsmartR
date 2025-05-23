### Create the standard datasets for enriching and report creating.
OG_dir = getwd() ; setwd('/Users/jakepowell/Desktop/enrich')
path_to_TreeSearch = '/Users/jakepowell/Downloads/global_tree_search_trees_1_8.csv' # Version 1.8.
path_to_wcvp_names = '/Users/jakepowell/Downloads/wcvp/wcvp_names.csv' # Version 13.
path_to_wcvp_distribution = '/Users/jakepowell/Downloads/wcvp/wcvp_distribution.csv' # Version 13.

# IUCN RedList uses the rredlist package (API) to retrieve RedList information. We attached a date to the .rda file informing of when the redlist information was taken from the website.
# BGCI PlantSearch information is scrapped from it's website. We attached a date to the .rda file informing of when BGCI PlantSearch was scraped.

#######################
### 1) World Checklist of Vascular Plants (solo)
#######################
wcvp = BGSmartR::import_wcvp_names(filepath = path_to_wcvp_names, wanted_columns = c(
  "plant_name_id", "ipni_id", "taxon_rank", "taxon_status", "family", "genus", "species_hybrid",
  "species", "infraspecific_rank", "infraspecies", "parenthetical_author", "primary_author",
  "publication_author", "nomenclatural_remarks", "geographic_area", "lifeform_description",
  "climate_description", "taxon_name", "taxon_authors", "accepted_plant_name_id","basionym_plant_name_id","homotypic_synonym","powo_id"))
save(wcvp, file = 'wcvp_names.rda')

wcvp = BGSmartR::add_wcvp_distributions(filepath = path_to_wcvp_distribution, wcvp = wcvp)
save(wcvp, file = 'wcvp_names_and_distribution.rda')
#######################

#######################
### 2) BGCI's PlantSearch
#######################
# Scrapey scrape.
#######################

#######################
### 3) IUCN RedList
#######################
# 1) Enter your redlist key.
key = "//KEY\\"

# 2) Use `rl_sp` function to get all records from red list
# (code taken from the documentation)
out <- rredlist::rl_sp(all = TRUE, key=key)
length(out)
vapply(out, "[[", 1, "count")
all_df <- do.call(rbind, lapply(out, "[[", "result"))

# 3) Restrict to only plants.
IUCN_redlist = all_df[all_df$kingdom_name == "PLANTAE",]

# 4) Prepare the data.
IUCN_redlist = BGSmartR::prepare_enrich_database(enrich_database = IUCN_redlist,
                                                 enrich_taxon_name_column = 'scientific_name',
                                                 enrich_taxon_authors_column = 'taxonomic_authority',
                                                 do_add_id = FALSE,
                                                 console_message = TRUE)

save(IUCN_redlist, file = paste0('IUCN_redlist_', Sys.Date(), '.rda'))
#######################

#######################
### 4) BGCI's GlobalTreeSearch
#######################
# To get GlobalTreeSeach we download it from the BGCI website. Currently -> https://tools.bgci.org/global_tree_search.php and version 1.8 of the dataset.

# Option 1: Just use the downloaded data (simplier)
GlobalTreeSearch=read.csv(path_to_TreeSearch)[,1:2]
names(GlobalTreeSearch) = c('taxon_names', 'taxon_author')
GlobalTreeSearch = BGSmartR::prepare_enrich_database(GlobalTreeSearch,
                                               enrich_taxon_name_column = 'taxon_names',
                                               enrich_taxon_authors_column = 'taxon_author')
save(GlobalTreeSearch, file = 'GlobalTreeSearch.rda')
# We can now use GlobalTreeSearch to enrich your collection.

# Option 2: match GlobalTreeSearch to WCVP to get tree symonyms.
GlobalTreeSearch=read.csv(path_to_TreeSearch)[,1:2]
names(GlobalTreeSearch) = c('taxon_names', 'taxon_author')
GlobalTreeSearch$taxon_name_full = paste0(GlobalTreeSearch$taxon_name, ' ', GlobalTreeSearch$taxon_author)

# Load the WCVP dataset to match GlobalTreeSearch ato.
# load('wcvp_names_and_distribution.rda')
enriched_GlobalTreeSearch = BGSmartR::enrich_collection(collection = GlobalTreeSearch,
                                                  wcvp = wcvp,iucnRedlist = NA, BGCI = NA,
                                                  enrich_taxon_authors_column = 'taxon_authors',
                                                  taxon_name_column = 'taxon_names',
                                                  # taxon_name_full_column = 'taxon_name_full',
                                                  taxon_author_column = 'taxon_author',
                                                  do_is_autonym = FALSE,
                                                  do_status_year = FALSE,
                                                  do_taxon_types = FALSE,
                                                  typo_method = 'Data frame only')

### Get the unique plant name identifiers of the matched trees in WCVP
matched_plant_ids = unique(enriched_GlobalTreeSearch$POWO_plant_name_id)
matched_plant_ids = matched_plant_ids[!is.na(matched_plant_ids)]

#Find all plant name identifiers with accepted plant identifier in matched_plant_ids.
all_ids = wcvp$wcvp_names$plant_name_id[wcvp$wcvp_names$accepted_plant_name_id %in% matched_plant_ids]

# Reduce WCVP to the wanted identifiers (all_ids) and select taxon name and author columns.
all_wcvp_trees = wcvp$wcvp_names[match(all_ids, wcvp$wcvp_names$plant_name_id),match(c('taxon_name', 'taxon_authors'), names(wcvp$wcvp_names))]
# Remove all infraspecific parts.
all_wcvp_trees$taxon_name  = stringr::str_remove(all_wcvp_trees$taxon_name, pattern = ' [a-zA-z]*\\..*')
# Remove all hybridisations.
all_wcvp_trees$taxon_name  = stringr::str_remove(all_wcvp_trees$taxon_name, pattern = '\\+ |\u00D7 ')
# Add taxon name and author combined.
all_wcvp_trees$taxon_name_full = paste0(all_wcvp_trees$taxon_name, ' ', all_wcvp_trees$taxon_authors)

# Update column names
names(all_wcvp_trees) = names(GlobalTreeSearch)
#combine GlobalTreeSearch and all_wcvp_trees.
all_trees = rbind(GlobalTreeSearch,all_wcvp_trees)

# Remove duplicates.
all_trees = all_trees[match(unique(all_trees$taxon_name_full), all_trees$taxon_name_full),]


trees_database = BGSmartR::prepare_enrich_database(all_trees,
                                                   enrich_taxon_name_column = 'taxon_names',
                                                   enrich_taxon_authors_column = 'taxon_author')


trees_database$is_tree = rep(TRUE,nrow(trees_database))

# Column `from` stating which database the name came from
from = rep('',nrow(trees_database))
from[which(trees_database$taxon_name_full %in% GlobalTreeSearch$taxon_name_full)] = 'BGCI'
from[which(trees_database$taxon_names %in% all_wcvp_trees$taxon_names)] = paste0(
  from[trees_database$taxon_names %in% all_wcvp_trees$taxon_names],
  ' WCVP')
trees_database$from = from

save(trees_database, file = 'trees_database.rda')
#######################

#######################
### 5) Enhance WCVP with other datasets
#######################
wcvp_names = wcvp$wcvp_names
# Only want to match those which are accepted or do not have an accepted name.
wanted_index = c(which(wcvp_names$plant_name_id %in% unique(wcvp_names$accepted_plant_name_id)),
                 which(is.na(wcvp_names$accepted_plant_name_id)))

# Extact the taxon name and author.
wcvp_care = data.frame(plant_name_id = wcvp_names$plant_name_id[wanted_index],
                       taxon_name = wcvp_names$taxon_name[wanted_index],
                       taxon_author = wcvp_names$taxon_authors[wanted_index])

# Load the red list.
load('/Users/jakepowell/Desktop/enrich/IUCN_redlist_2024-09-12.rda') # Add filepath to IUCN redlist if needed to re-load
redList = IUCN_redlist
redList$taxon_status = rep('NA',nrow(redList))
redList$accepted_plant_name_id = 1:nrow(redList)
redList$plant_name_id = 1:nrow(redList)

match_info = BGSmartR::match_collection_to_iucnRedlist(collection = wcvp_care,
                                                       iucnRedlist = redList,
                                                       taxon_name_column = "taxon_name",
                                                       taxon_author_column = "taxon_author",
                                                       typo_method = 'Data frame only'
                                                       )
save(match_info, file = 'wcvp_match_IUCNred.rda')

found_match = which(match_info$match > 0)
POWO_redlist = wcvp_care[found_match,]

corres_redlist = IUCN_redlist[match_info$match[found_match],]
POWO_redlist = data.frame(POWO_redlist, corres_redlist)

### Version if you have the simple version of the red list.
wanted_info = c('plant_name_id', 'taxon_name', 'taxon_author', 'taxonid', 'kingdom_name', 'phylum_name', 'class_name', 'order_name', 'family_name', 'genus_name', 'scientific_name', 'taxonomic_authority', 'infra_rank', 'infra_name', 'population', 'category', 'main_common_name', 'sanitise_name', 'sanitise_author')

### Version if you get the more detailed version of the red list.
# wanted_info = c("plant_name_id", "taxon_name", "taxon_author", "taxonid", "scientific_name",
#                 "phylum", "class", "order", "family", "genus", "main_common_name", "authority",
#                 "published_year", "assessment_date", "category", "criteria", "population_trend",
#                 "marine_system", "freshwater_system", "terrestrial_system", "assessor", "reviewer",
#                 "aoo_km2", "eoo_km2", "elevation_upper", "elevation_lower", "depth_upper",
#                 "depth_lower","errata_flag", "errata_reason", "amended_flag", "amended_reason")

POWO_redlist = POWO_redlist[,match(wanted_info, names(POWO_redlist))]

wcvp$redList = POWO_redlist
save(wcvp, file = 'wcvp_with_redlistcategory.rda')

### Add Trees to wcvp
wcvp_genus_species = paste0( wcvp$wcvp_names$genus, ' ',wcvp$wcvp_names$species)
tree_ids = wcvp$wcvp_names$plant_name_id[which(wcvp_genus_species %in% trees_database$taxon_name)]


wcvp$trees = data.frame(plant_name_id = tree_ids)
save(wcvp, file = 'wcvp_with_redlist_tree.rda')


#######################

setwd(OG_dir)
