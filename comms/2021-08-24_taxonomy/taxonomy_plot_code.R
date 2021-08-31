# code to plot taxonomy data using data.tree terminology
# depends on galah v1.3.0+

# load packages etc
library(galah)
library(data.tree)
library(stringr)
library(ggplot2)
library(viridisLite)
source("functions_circles.R")

# get data from the ALA
chordate_orders <- ala_taxonomy(select_taxa("chordata"), down_to = "order")

# restrict to data from the Australian Faunal Directory
Prune(chordate_orders, pruneFun = function(a){a$authority == "AFD"})
tree <- prep_tree(prune_leafless_branches(chordate_orders))

# Extract data.frame with 1 row per node (i.e. not just leaves)
# and including leaf_min and leaf_max (calculated above using prep_tree())
tree_df <- ToDataFrameTree(tree,
   "name", "rank", "level", "is_leaf", "leaf_min", "leaf_max")

# convert start & end values to percentages
max_value <- max(tree_df$leaf_max)
tree_df$segment_start <- ((tree_df$leaf_min - 1) * 360) / max_value
tree_df$segment_end <- ((tree_df$leaf_max) * 360) / max_value

# create an argument to tell segment function how many points to make
df_higher <- tree_df[!tree_df$is_leaf, ]
df_higher <- df_higher[-1, ] # remove root

# create segments as polygons
segment_df <- get_segments(df_higher)
segment_df$rank_factor <- factorize_ranks(segment_df)

# calculate rotated text to segments
segment_text_df <- get_segment_text(df_higher)

# calculate rotated text for leaves
leaf_text_df <- get_leaf_text(tree_df)

# plot
p <- ggplot() +
  geom_polygon(
    data = segment_df,
    mapping = aes(x = x, y = y, group = group, fill = rank_factor),
    color = "white"
  ) +
  geom_text(
    data = tree_df[1, ], 
    mapping = aes(label = name),
    x = 0,
    y = 0,
    hjust = 0.5,
    color = "grey30",
    family = "mono") +
  geom_text(
    data = segment_text_df,
    mapping = aes(x = x, y = y, label = label, angle = rotation),
    size = 3,
    color = "white",
    family = "mono"
  ) +
  geom_text(
    data = leaf_text_df,
    mapping = aes(x = x, y = y, label = label, angle = rotation, hjust = hjust),
    size = 3,
    color = "grey50",
    family = "mono"
  ) +
  lims(x = c(-10, 8), y = c(-10, 8))+
  labs(fill = "Rank") +
  scale_fill_manual(values = c(viridis(5, begin = 0, end = 0.9), "grey40")) +
  # scale_fill_brewer(palette = "Dark2") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = c(0.9, 0.12))

ggsave("./plots/sunburst.pdf", p, width = 20, height = 20, units = "cm")
