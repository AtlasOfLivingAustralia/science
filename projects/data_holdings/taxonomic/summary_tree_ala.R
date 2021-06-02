## ---------------------------------------------------------#
## Title: Summary tree of ALA Taxonomy based on occurrences
## Author: Dax Kellie
## Date Created: 2021-06-2
## ---------------------------------------------------------#


#| This code uses the summarytrees package to display higher classifications
#| For more information:
#| 
#| https://iphylo.blogspot.com/2021/05/maximum-entropy-summary-trees-to.html
#| https://github.com/kshirley/summarytrees
#| http://www.kennyshirley.com/summarytrees/


rm(list = ls())

# packages
library(galah)
library(tidyverse)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)


#----------------------------------------------------------#
#                         Get Data
#----------------------------------------------------------#

# Call saved data file

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Download data
#| This data was downloaded from ALA using the following code:
#___________________________________________________________________________________________#
## Get counts down to family level
# families <- ala_counts(group_by = "family", limit = 6416) # use counts to find all families
# family_names <- dplyr::pull(families, family) # extract list of all family names
#| Note: We use ala_counts() 1st, not select_taxa() because ala_counts() retains all levels
#| of taxonomy


## Find counts
# counts_ala <- select_taxa(term = family_names, counts = TRUE) # takes ~10 mins to run
## saveRDS(counts_ala, file = "data/df_familycounts_ala.rds")
#___________________________________________________________________________________________#

# Load data from folder
counts_ala <- readRDS(file = "data/df_familycounts_ala.rds") # load family counts



#----------------------------------------------------------#
#                       Wrangle Data
#----------------------------------------------------------#

dt_counts_ala <- lazy_dt(counts_ala) # convert to lazy data.table for dtplyr

dt_counts_ala <- dt_counts_ala %>% 
  select(kingdom, phylum, class, order, family, count) %>% # select cols we want
  arrange(kingdom, phylum, class, order, family, count) %>% # sort alphabetically/numerically
  as.data.table() 

count_df <- as.data.frame(dt_counts_ala)[apply(dt_counts_ala[, 1:5], 1, function(a){!any(is.na(a))}), ]

count_df <- rename(count_df, OBS = count) # have to rename because "count" is also a command in R
count_df %>% arrange(desc(OBS)) %>% top_n(10) # What we are expecting



#----------------------------------------------------------#
#           Convert dataframe to tree format
#----------------------------------------------------------#

# Create string names of full taxonomic names
count_df_tree <- count_df %>% mutate(
  pathString = paste("Top",
                     .$kingdom,
                     .$phylum,
                     .$class,
                     .$order,
                     .$family,
                     sep = "/")
)


#| Note: This code is taken from the supplementary material available at: 
#| https://github.com/kshirley/summarytrees

#| This code was edited from the DMOZ.Rmd file, included in the vignettes folder,
#| available in the git repository


#------------- initialize variables---------------#

full.node <- (paste0(c(count_df_tree$pathString)))  # full node names with slashes
weight <- as.integer(count_df_tree$OBS) # Use number of OBS as weight

parent <- NULL  # the vector of all parents
n.added <- NULL  # how many new internal nodes added each iteration
iter <- 0  # count iterations (just out of curiosity)
to.split <- full.node  # the set of full names to split
new.parent <- rep(NA, length(weight))  # parents of each round of nodes

t1 <- Sys.time()
while (sum(is.na(new.parent)) > 0) {
  iter <- iter + 1
  print(iter)
  
  # split by slash and extract the leaf label of each node, and the 'stem'
  split.node <- strsplit(to.split, "/")
  label <- sapply(split.node, function(x) tail(x, 1))
  stem <- sapply(split.node, function(x) paste(x[-length(x)], collapse = "/"))
  
  # compute the parent of each node:
  new.parent <- match(stem, full.node)
  
  # if new.parent is NA, then we have to add an internal node
  # get unique internal nodes that must be added
  new.internal.nodes <- unique(stem[is.na(new.parent)])
  n.added <- c(n.added, length(new.internal.nodes))
  # add the new internal nodes to the full list
  full.node <- c(full.node, new.internal.nodes)
  # internal nodes have a weight of zero by definition here
  weight <- c(weight, rep(0, length(new.internal.nodes)))
  # set up the next set of nodes whose parents must be found
  to.split <- new.internal.nodes
  # add to the vector of parents
  parent <- c(parent, match(stem, full.node))
}
t2 <- Sys.time()
t2 - t1



#----------------------------------------------------------#
#           Compute labels, assemble nodes
#----------------------------------------------------------#

label <- sapply(strsplit(full.node, "/"), function(x) tail(x, 1))

# There should be one that is the 'parent' of the root, which is an empty node
# Give it a label of NA
label[sapply(label, length) == 0] <- NA
label <- unlist(label)

# Pull it all into a data.frame:
tree <- data.frame(node = 1:length(full.node),
                   parent = parent,
                   weight = weight,
                   label = label,
                   stringsAsFactors = FALSE)

# identify the 'parent' of the root, which doesn't really exist:
to.remove <- which(is.na(tree[, "label"]))

# Set the parent of the root to zero
tree[tree[, "parent"] == to.remove, "parent"] <- 0

# remove the 'parent' of the root
tree <- tree[-to.remove, ]

# Save data frame (if you would like):
# save(tree, file = "data/tree.RData")



#----------------------------------------------------------#
#                       Run model
#----------------------------------------------------------#
# get summary trees package
library(devtools)
# install_github("kshirley/summarytrees", build_vignettes = FALSE)
library(summarytrees)
data(tree)

# Look at the data a bit:
dim(tree)
tree[1:10, ]

# Look at the breakdown between internal nodes vs. leaves:
table(tree[, "weight"] > 0)

# compute a set of K summary trees:
t1 <- Sys.time()
K <- 50
g <- greedy(node = tree[, "node"],
            parent = tree[, "parent"],
            weight = tree[, "weight"],
            label = tree[, "label"],
            K = K)
t2 <- Sys.time()
t2 - t1


# See list of top 20
g$summary.trees[[20]]



#----------------------------------------------------------#
#                     Visualization
#----------------------------------------------------------#

# Prepare the summary trees for visualization:
json <- prepare.vis(tree.list = g$summary.trees,
                    labels = g$data[, "label"],
                    tree = g$tree,
                    legend.width = 150,
                    node.width = 225,
                    node.height = 14,
                    units = "# of Observations",
                    print.weights = TRUE,
                    legend.color = "lightsteelblue",
                    color.level = 3)

# Serve the vis in the browser
draw.vis(json.object = json,
         out.dir = tempfile(),
         open.browser = interactive())





