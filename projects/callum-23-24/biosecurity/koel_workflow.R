library(tidyverse)
library(ggflowchart)
library(igraph)

node_names <- tibble(
  name = as.character(1:13),
  label = c("List Folder", #1
            "Shapefiles Folder", #2
            "collate_lists()", #3
            "get_species_lists2()", #4
            "assign_common_names()", #5
            "get_occurrences()", #6
            "build_email()", #7
            "clean_names()", #8
            "identify_shape()", #9
            "galah_field_search()", #10
            "build_gt_table()", #11
            "build_map_thumbnails()", #12
            "send_email()" #13
  ),
  type = c("Input",
           "Input",
           "Workflow Function",
           "Workflow Function",
           "Workflow Function",
           "Workflow Function",
           "Workflow Function",
           "Internal Function",
           "Called Function",
           "Internal Function",
           "Called Function",
           "Called Function",
           "Called Function"
  )
)

arrows <- tibble(
  from = as.character(
          c(1, 3, 4, 5, 4, 6, 4, 2, 6, 6,  7,  7,  7)),
    to = as.character(
          c(3, 4, 5, 6, 6, 7, 8, 9, 9, 10, 11, 12, 13))
)

g <- igraph::graph_from_data_frame(
  select(arrows, c(from, to)),
  directed = TRUE)
coords <- igraph::layout_as_star(g)
colnames(coords) <- c("x", "y")
nodes <- tibble::as_tibble(coords) %>%
  mutate(name = igraph::vertex_attr(g, "name")) %>%
  left_join(node_names, by = "name")

ggflowchart(arrows, node_data = nodes, layout = "custom")
