occurrence_download <- function(ibra, taxon){
  cat(paste0("downloading data for taxon = ", taxon, ", ibra = ", ibra))

  x <- ala_occurrences(
    taxa = select_taxa(taxon),
    filters = select_filters(cl1048 = ibra, profile = "ALA"),
    columns = select_columns(
      "species_guid",
      "decimalLatitude",
      "decimalLongitude",
      "cl1048", # IBRA 7 regions
      "cl1049", # IBRA 7 sub-regions
      "eventDate",
      "year",
      "basisOfRecord",
      "data_resource_uid"))

  # clean up
  x <- x[!is.na(x$species_guid), ]
  x <- x[x$species_guid != "", ]

  saveRDS(x,
    paste0("./cache/taxon-", taxon, "_ibra-", gsub(" ", "-", ibra), ".rds"))
}


calculate_specaccum <- function(file){
  cat(paste("reading file ", file, " ... "))
    x <- readRDS(file)

  if(nrow(x) > 10){ # criterion from La Sorte & Somveille 2020 (10.1111/ecog.04632)

    # get list of species
    species_list <- sort(unique(x$species_guid))
    species_df <- data.frame(species_guid = species_list)

    # convert to wide format
    col_list <- lapply(species_list, function(a){
      as.numeric(x$species_guid %in% a)
    })
    obs_matrix <- do.call(cbind, col_list)

    # Note: check vegan::specpool for a better option

    # # or convert to year by species matrix
    # year_count_list <- lapply(
    #   split(x, x$year),
    #   function(a){
    #     xt_df <- as.data.frame(xtabs(~ species_guid, data = a))
    #     result <- merge(species_df, xt_df, all = TRUE)
    #     result$Freq[is.na(result$Freq)] <- 0
    #     return(result$Freq)
    #   })
    # obs_matrix_year <- as.data.frame(do.call(rbind, year_count_list))
    ## in practice, the shape of the SAC is identical with this method

    # calculate model
    cat("calculating model ... ")
    spp_accum <- vegan::specaccum(obs_matrix, method = "exact")
    # spp_accum_year <- vegan::specaccum(obs_matrix_year, method = "exact")
    # plot(spp_accum)
    # quartz(); plot(spp_accum_year)
    cat("done\n")
    return(spp_accum)
    # note: sac$freq has frequency per species; ergo length(sac$freq) = n_species

  }else{
    cat("skipping model ... done\n")
    return(NULL)
  }
}


vegan_models <- function(df){
  list(
    arrhenius = try(
      nls(richness ~ SSarrhenius(sites, k, z), data = df),
      silent = TRUE),
    gleason = try(
      nls(richness ~ SSgleason(sites, k, slope), data = df),
      silent = TRUE),
    gitay = try(
      nls(richness ~ SSgitay(sites, k, slope), data = df),
      silent = TRUE),
    lomolino = try(
      nls(richness ~ SSlomolino(sites, Asym, xmid, slope), data = df),
      silent = TRUE),
    asymp = try(
      nls(richness ~ SSasymp(sites, Asym, R0, lrc), data = df),
      silent = TRUE),
    weibull = try(
      nls(richness ~ SSweibull(sites, Asym, Drop, lrc, pwr), data = df),
      silent = TRUE)
  )
}


# calculate 'true' richness under doubled survey effort
extrapolate_richness <- function(model){

  if(!is.null(model)){

    model_data <- data.frame(
      sites = model$sites,
      richness = model$richness)
    # note: this is chosen because it doesn't underestimate richness
    # BUT it also doesn't asymptote
    model_arrhenius <- nls(richness ~ SSarrhenius(sites, k, z),
      data = model_data)
    df <- data.frame(extrapolated_richness = predict(
      model_arrhenius,
      newdata = data.frame(sites = max(model_data$sites) * 2)
    ))
    df$observed_richness <- length(model$freq)
    df$richness_deficit <- df$extrapolated_richness - df$observed_richness
    df$proportion_complete <- df$observed_richness / df$extrapolated_richness

  }else{
    df <- data.frame(
      extrapolated_richness = NA,
      observed_richness = 0,
      richness_deficit = NA,
      proportion_complete = 0)
  }
  return(df[, c(2, 1, 3, 4)])
}


# optionally plot SACs with a variety of forms for a single model
extrapolate_SAC <- function(model, multiplier = 2){

  model_data <- data.frame(
    sites = model$sites,
    richness = model$richness)

  # run a series of models, remove those that don't fit
  models <- vegan_models(model_data)
  models <- models[unlist(lapply(models, function(a){class(a) != "try-error"}))]

  ## optional - compare fit by AICc
  ## This is the approach used by Mora et al. 2008
  ## Note: this seems to select models that underestimate the asymptote
  # model_table <- AICcmodavg::aictab(models, sort = FALSE)
  # model_nls <- models[[which.max(model_table$AICcWt)]]

  # predict richness at a given level of effort
  df <- data.frame(sites = seq(1,
    max(model_data$sites) * multiplier,
    length.out = 100))

  model_predictions <- rbind(
    data.frame(
      sites = rep(df$sites, length(models)),
      model = rep(names(models), each = nrow(df)),
      fit = do.call(c,
        lapply(models, function(a){predict(a, newdata = df)}))),
    data.frame(
      sites = model_data$sites,
      model = "SAC",
      fit = model_data$richness
    ))

  return(model_predictions)
}
