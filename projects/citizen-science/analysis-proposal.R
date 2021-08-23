## ------------------------------------------------------#
## Title: Citizen Science Analysis Proposal
## Author: Dax Kellie
## Date Created: 2021-08-11
## ------------------------------------------------------#

# packages
library(galah)
library(tidyverse)
library(purrr)
library(viridis)

search_fields("data")
find_field_values("dataResourceName")


# provider_counts <- ala_counts(group_by = "dataProviderName")
resource_counts <- ala_counts(group_by = "dataResourceName", limit = 900)
resource_counts %>% slice(1:20)


# Find number of records by data provider by year

# list years
years <- 1999:2003
years_long <- paste0("select_filters(year = ", years, ")", sep="")

# Get counts of records from data providers by year
data_providers <- years_long %>%
  map(~ala_counts(
    list(filters = .x),
    group_by = "dataResourceName",
    limit = 900)) %>%
  tibble(year = years,
         y = .) %>%
  unnest(y) # back to df

# clean string names of data providers
data_providers <- data_providers %>%
  mutate(dataResourceName = str_squish(dataResourceName))

# plot
data_providers %>%
  drop_na() %>%
  group_by(dataResourceName) %>%
  # arrange(year, dataResourceName) %>%
  dplyr::summarise(count = count,
                   total = sum(count),
                   cum_total = cumsum(count),
                   year = year) %>%
  filter(total > 30000) %>%
  ggplot(aes(x = year,
             y = cum_total,
             fill = reorder(str_wrap(dataResourceName), -cum_total))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = "Year", y = "Number of Records") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  guides(fill = guide_legend(title = "Data Providers", nrow = 6)) +
  theme_minimal() +
  theme(legend.position = "top")




#-------------------------------------------------------------------------------
# by record type
record_type <- years_long %>%
  map(~ala_counts(
    list(filters = .x),
    group_by = "basisOfRecord",
    limit = 900)) %>%
  tibble(year = years,
         y = .) %>%
  unnest(y) # back to df

record_type <- record_type %>%
  mutate(basisOfRecord = stringr::str_to_title(basisOfRecord))

record_type %>%
  drop_na() %>%
  group_by(basisOfRecord) %>%
  dplyr::summarise(count = count,
                   total = sum(count),
                   cum_total = cumsum(count),
                   year = year) %>%

  ggplot(aes(x = year,
             y = cum_total,
             fill = reorder(basisOfRecord, -cum_total))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::label_number_si()) +
  labs(x = "Year", y = "Number of Records") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  guides(fill = guide_legend(title = "Basis of Record")) +
  theme_minimal()


#-------------------------------------------------------------------------------

# Ok now by kingdom

# Extract kingdom names
kingdoms <- ala_counts(group_by = "kingdom", limit = 10)
kingdom_names <- pull(kingdoms, kingdom)

# What martin did to get them all (but his wasn't by year)
kingdom_counts <- kingdom_names %>%
  map( ~ ala_counts(
    taxa = select_taxa(list(kingdom = .x)),
    group_by = "dataResourceName",
    limit = 10
  )) %>%
  tibble(
    kingdom = kingdom_names,
    y = .) %>%
  unnest(y)


# I think I just have to put this into a function and run it many times

# We have to do it this way because each select_filters() call builds a df
year_filter_1999 <- select_filters(year = 1999)
year_filter_2000 <- select_filters(year = 2000)
year_filter_2001 <- select_filters(year = 2001)
year_filter_2002 <- select_filters(year = 2002)
year_filter_2003 <- select_filters(year = 2003)

# Create a function that gets counts for all kingdoms and converts to data frame
get_year_counts <- function(year_filter, year_number) {
  kingdom_counts <- kingdom_names %>%
    map( ~ ala_counts(
      taxa = select_taxa(list(kingdom = .x)),
      filters = year_filter,
      group_by = "dataResourceName",
      limit = 900
    )) %>%
    tibble(
      kingdom = kingdom_names,
      year = rep(year_number),
      y = .) %>%
    unnest(y) %>% select(-name)
}

# Run this function for each year
counts_1999 <- get_year_counts(year_filter_1999, 1999)
counts_2000 <- get_year_counts(year_filter_2000, 2000)
counts_2001 <- get_year_counts(year_filter_2001, 2001)
counts_2002 <- get_year_counts(year_filter_2002, 2002)
counts_2003 <- get_year_counts(year_filter_2003, 2003)

# merge all the years
counts_99_to_03 <- rbind(counts_1999, counts_2000, counts_2001, counts_2002, counts_2003)


# Get complete list of possible kingdoms and dataResources

# We have kingdom names
kingdom_names

# Extract resources names
resources <- find_field_values("dataResourceName", limit = 900)
resource_names <- pull(resources, category)
resource_names <- resource_names %>% str_trim() # trim whitespace


# Use crossing() to find all possible combinations of inputs
years_list <- 1999:2003
king_df <- crossing(kingdom_names, years_list, resource_names)

king_df <- king_df %>% # Do some renaming before merging
  rename(kingdom = kingdom_names,
         year = years_list,
         dataResourceName = resource_names)

# Get missing values
full_counts <- king_df %>%
  left_join(counts_99_to_03) %>%
  replace_na(list(count = 0)) # replace NAs

full_counts %>% slice(1:10)
full_counts <- full_counts %>%
  group_by(kingdom, dataResourceName) %>%
  mutate(
    cum_count = cumsum(count)
  )

full_counts %>% filter(kingdom == "Fungi") %>% filter(dataResourceName == "Fungimap")

#------------ Plot ---------------#
library(ggstream)
library(viridis)


full_counts_tidy <- full_counts %>%
  mutate(across(where(is.character), as.factor))

glimpse(full_counts_tidy)

# Plotting stacked area chart function
stream_plot_function <- function(kingdom_name, at_least_this_many_records){
  full_counts_tidy %>%
    drop_na() %>%
    filter(kingdom == as.character(kingdom_name)) %>%
    group_by(kingdom, dataResourceName) %>%
    dplyr::summarise(total = cumsum(count),
                     year = year) %>%
    filter(total > at_least_this_many_records) %>% # filter records

    ggplot(aes(x = year, # Plot
               y = total,
               fill = str_wrap(dataResourceName))) +
    geom_stream(color = "black",
                extra_span = .013,
                type = "ridge", bw = 1) +
    # geom_stream_label(aes(label = dataResourceName),
    #                   size = 4, type = "ridge",
    #                   colour = "black") +
    scale_fill_viridis_d(option = "D") +
    guides(fill = guide_legend(title = "Data provider")) +
    scale_x_continuous(name = "Year") +
    scale_y_continuous(name = "Total observations per year",
                       labels = scales::comma) +
    theme_minimal()
}



full_counts_tidy %>% group_by(kingdom) %>% summarise(count = n()) # list kingdoms


# Select kingdom and plot
stream_plot_function("Animalia", 200000)
stream_plot_function("Fungi", 5000)


#-------------------------------------------------------------------------------
# Calculate rate of change in observations by year

rate_of_change <- full_counts %>%
  group_by(kingdom, dataResourceName) %>%
  mutate(pct_change = (cum_count/lead(count)))

rate_of_change$pct_change[is.nan(rate_of_change$pct_change)]<-0

rate_of_change %>% filter(kingdom == "Fungi") %>% filter(dataResourceName == "Fungimap")

rate_of_change %>% filter(kingdom == "Fungi") %>% filter(count > 1) %>%
  ggplot(aes(x = year, fill = dataResourceName)) +
  geom_bar(aes(y = pct_change), stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(legend.position = "none")


#-------------------------------------------------------------------------------

#Ideas
#   | Number of unique observations over time



#-------------------------------------------------------------------------------
#   | Visualise influx of observations for popular species vs unpopular species - Top 10-25 species vs the rest of the ALA


#   | Rate of observations in bushfire areas before vs after bushfires


#   | Could check number of papers published using ALA species distributions over time

