# functions to support taxonomic data extraction from PDFs

taxon_frequencies <- function(file_path){
  x <- namext::name_extract(file_path)$names
  
  if(nrow(x) > 0){
    # find out whether any names are contractions of earlier names in same text
    # if so, replace with 'full' names
    full_species_names <- unique(x$name[
      !grepl("^[[:upper:]]\\.\\s[[:lower:]]+", x$name) &
       grepl("^[[:alpha:]]+\\s[[:alpha:]]+$", x$name)
    ]) 
    contracted_names <- unlist(lapply(
      strsplit(full_species_names, " "),
      function(b){paste(substr(b[[1]], 1, 1), b[[2]], sep = ". ")}
    ))
    names_lookup <- data.frame(
      name = contracted_names, replacement = full_species_names)
      
    if(nrow(names_lookup) > 0){
      x <- left_join(x, names_lookup)
      if(any(!is.na(x$replacement))){
        x$name[!is.na(x$replacement)] <- x$replacement[!is.na(x$replacement)]
      }
    }

    # group by name and return counts
    x_clean <- x |>
      group_by(name) |>
      summarise(n = n(), file = file_path) |>
      filter(n > 2) |> # remove singletons and doubletons
      arrange(desc(n))
      
    if(nrow(x_clean) > 0){
    
      # compare vs ALA dataset, return only exact matches
      y <- search_taxa(x_clean$name) |> 
        as_tibble()
      
      if(ncol(y) > 1 & nrow(y) > 0){  
        # join to return only those names that are matched in ALA
        y <- y |> filter(match_type == "exactMatch")
        result <- inner_join(x_clean, y, by = c("name" = "search_term")) 
        
        # check no column names are missing
        name_check <- column_names() %in% colnames(result)
        if(!all(name_check)){
          missing_cols <- column_names()[!name_check]
          for(i in seq_along(missing_cols)){
            result[[missing_cols[i]]] <- as.character(NA)        
          }
        }
        result |>
          select(file, name, n, 
            rank, taxon_concept_id, scientific_name,
            kingdom, phylum, class, order, family, genus, species)
      }else{
        empty_taxonomy(file_path)
      }
    }else{ 
      empty_taxonomy(file_path)
    }       
  }else{
    empty_taxonomy(file_path)
  }
}

get_taxonomy_info <- function(file_paths){
  # extract_list <- list()
  # for(i in seq_along(file_paths)){
  #   extract_list[[i]] <- taxon_frequencies(file_paths[i])
  # }
  extract_list <- lapply(file_paths, taxon_frequencies)
  do.call(rbind, extract_list) |> as_tibble()
}

empty_taxonomy <- function(file_path){
  result <- as.data.frame(matrix(NA, nrow = 1, ncol = 13))
  colnames(result) <- column_names()
  result$file <- file_path
  result
}

column_names <- function(){
  c("file", "name", "n", 
    "rank", "taxon_concept_id", "scientific_name",
    "kingdom", "phylum", "class", "order", "family", "genus", "species")
}
