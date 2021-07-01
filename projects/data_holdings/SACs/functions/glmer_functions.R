
count_function <- function(a, taxon){
  filter <- select_filters(decade = a$decade, cl1048 = a$ibra)
  return(ala_counts(taxa = taxon, filters = filter))
}
 
  
data_function <- function(a, taxon){
  filter <- select_filters(decade = a$decade, cl1048 = a$ibra)
  if(a$n_records > 0){
    species <- ala_species(taxa = taxon, filters = filter)$species_name
    if(length(species) > 0){
      return(data.frame(
        ibra = a$ibra,
        decade = a$decade,
        n_records = a$n_records,
        species = species
      ))
    }else{NULL}
  }else{NULL}
}
