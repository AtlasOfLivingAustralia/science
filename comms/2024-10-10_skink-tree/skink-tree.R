
library(galah)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(data.tree)

galah_config(email = "dax.kellie@csiro.au")

skink_taxonomy <- galah_call() |>
  identify("Lygosominae") |>
  filter(rank == "subspecies") |>
  atlas_taxonomy()

skink_taxonomy_edit <- skink_taxonomy |>
  mutate(
    genus = case_when(
      rank == "genus" ~ name,
      rank == "species" ~ stringr::word(name, 1),
      rank == "subspecies" ~ stringr::word(name, 1),
      .default = NA_character_
    )
  ) 

set_graph_style(plot_margin = margin(1,1,1,1))

# create tree from parent/child nodes
my_nodes <- skink_taxonomy_edit |>
  select(name, rank, genus) |>
  rename(
    taxa_name = name
  ) |>
  slice(-1)

my_edges <- skink_taxonomy_edit |>
  select(parent_taxon_concept_id, taxon_concept_id) |>
  rename(
    from = parent_taxon_concept_id,
    to = taxon_concept_id
  ) |>
  as_tbl_graph() |>
  activate(edges) |>
  data.frame() |>
  as_tibble() |>
  mutate_at(vars(from, to), as.character)

x <- tbl_graph(edges = my_edges)

my_edges3 <- skink_taxonomy_edit |>
  select(parent_taxon_concept_id, taxon_concept_id) |>
  rename(
    from = parent_taxon_concept_id,
    to = taxon_concept_id
  ) |>
  as_tbl_graph() |>
  activate(edges) |>
  mutate(
    name = skink_taxonomy_edit$name,
    rank = skink_taxonomy_edit$rank,
    genus = skink_taxonomy_edit$genus
  ) |> 
  activate(nodes) |>
  mutate(
    name = c("NA", skink_taxonomy_edit$name),
    rank = c("NA", skink_taxonomy_edit$rank),
    genus = c(NA, skink_taxonomy_edit$genus)
  ) |> 
  as_tbl_graph()

x <- tbl_graph(nodes = my_nodes)
y <- tbl_graph(edges = my_edges)

maybe <- x |>
  bind_graphs(y)

# tbl_graph(nodes = my_nodes, edges = my_edges3, directed = F)

# nodes <- skink_taxonomy |>
#   select(name, rank) |>
#   rename(
#     taxa_name = name
#   )

# This works but doesn't add nodes correctly
# test <- skink_taxonomy |>
#   rename(
#     from = parent_taxon_concept_id,
#     to = taxon_concept_id,
#     taxa_name = name
#   ) |>
#   as_tbl_graph()


skink_taxonomy |>
  distinct(genus) |>
  count()

plot <- my_edges3 |>
  # activate(nodes) |>
  ggraph(layout = 'dendrogram', circular = TRUE) + 
  geom_edge_elbow(aes(colour = factor(genus))) + 
  # geom_node_label(aes(label = class), repel = TRUE) +
  scale_edge_colour_viridis(discrete = TRUE, option = "H") +
  # theme_void() + 
  theme(legend.position = "none") +
  geom_node_text(aes(filter = leaf, label = name)) +
  coord_fixed()


# find out whether it was a leaf
leaves <- plot$data$leaf |> rbind() |> as.data.frame()

# convert to tibble
leaves_tibble <- as_tibble(cbind(taxon_concept_id = names(leaves), t(leaves))) |>
  rename(leaf = V2)

# get all them ids
leaves_list <- leaves_tibble |>
  filter(leaf == TRUE) |>
  pull(taxon_concept_id)

# create a function to return counts of each taxon id in a tibble
make_a_tibble <- function(taxon_id) {
  
  result <- galah_call() |>
    identify(taxon_id) |>
    group_by(scientificName) |>
    atlas_counts()
  
  tibble(
    id = {taxon_id},
    result
  )
}

# get all of the id tibbles and then smoosh them together
id_counts <- leaves_list |>
  purrr::map(make_a_tibble) |>
  bind_rows()

library(geomtextpath)
library(ggtext)

plot2 <- id_counts |>
  ggplot() +
  geom_bar(data = id_counts,
           aes(x = scientificName,
               y = count),
           stat = "identity") + 
  # geom_textpath(
  #   data = id_counts,
  #   aes(x = scientificName, y = count + 10, label = scientificName),
  #   angle = 90,
  #   hjust = 0,
  #   size = 2.3,
  #   color = "grey20") +
  # coord_polar() + 
  # pilot::theme_pilot(grid = "h") +
  coord_radial(inner.radius = 0.1, r.axis.inside = FALSE)

plot
plot2


# start with something new ------------------------------ #

# this might have been the wrong way to go about this...265 new packages is a lot
# install.packages("BiocManager")
# BiocManager::install(version = "3.19")
# BiocManager::install("YuLab-SMU/treedataverse")
library(data.tree)
library(ggtree)

library(dendextend)

skinky <- skink_taxonomy |>
  rename(
    taxa_name = name
  )

# TODO: Ask martin about getting correct format aka Labs post format
tree <- FromDataFrameNetwork(skinky |> 
                               slice(-1) |> 
                               select(parent_taxon_concept_id, taxon_concept_id, everything())
                             )

FromDataFrameTable(skinky |>
                     slice(-1) |>
                     select(parent_taxon_concept_id, taxon_concept_id))

x <- as.Node(skinky |>
          slice(-1) |>
          select(parent_taxon_concept_id,
                 taxon_concept_id, everything()),
        mode = "network")

str(x)

its_a_tree_now <- ToDataFrameTypeCol(x, "taxa_name")

its_a_tree_now %>%
  as_tibble() %>%
  mutate(
    level_1 = case_when(level_1 == skinky$taxon_concept_id, skinky$taxa_name, level_1),
    level_2 = ifelse(level_2 %in% skinky$taxon_concept_id, skinky$taxa_name, level_2)
         )
  # mutate(across(starts_with("level"), ~ ifelse(. %in% skinky$taxon_concept_id, skinky$taxa_name, .)))



ToDataFrameTypeCol(tree, "rank", "taxa_name", type = "rank")


ggtree(tree_col)

z <- igraph::graph_from_data_frame(skinky[,3:4])

pluck(z, !!!list(1))

str(z)

?FromDataFrameNetwork

lookup <- igraph::V(z)$name


# create a data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
edges=rbind(d1, d2)


