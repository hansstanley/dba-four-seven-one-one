############################################
###     Social Networks   ###
# install igraph package
install.packages("igraph")
# install RColorBrewer to make nice colors
install.packages("RColorBrewer")
# install tidyr for parsing metadata
install.packages("tidyr")
# load in the packages
library(igraph)
library(RColorBrewer)
library(tidyr)

### Part I: visualization                ###
# read in the nodes and edges data
edges <- read.table("amazon0302.txt", header=F, skip=4, sep="\t")

# names of columns to read from nodes metadata
columns <- c("Id", "title", "group")
# read raw Amazon products metadata into 1 column
raw_table <- read.table(
  "amazon-meta.txt",
  header=F, sep=";", stringsAsFactors=F, strip.white=T, skip=3, quote="",
  col.names=c("Raw")
)
# split each line into parts using ":" as the delimiter
# this takes a short while
raw_split <- strsplit(raw_table$Raw, ":\\s*")
# filter lines where the first part is one of the column names
raw_split <- raw_split[lapply(raw_split, "[[", 1) %in% columns]

# keys will correspond to column names
keys <- lapply(raw_split, "[[", 1)
# vals will correspond to the data in each row of the `nodes_df` data frame;
# concatenate the rest of the parts together with ":"
vals <- lapply(raw_split, function(parts) paste(parts[-1], collapse = ": "))
# optional, trim the white space from the values (quite slow)
# vals <- lapply(vals, trimws)
# create key-value data frame
nodes <- data.frame(
  Key=unlist(keys),
  Val=unlist(vals),
  stringsAsFactors=F
)

# assign a row number to each key-value pair,
# corresponding to a row in `nodes_df`
nodes$row <- cumsum(nodes$Key == "Id")
# rotate the key-value data frame into a proper table data frame
nodes_df <- pivot_wider(
  nodes,
  names_from=Key,
  values_from=Val,
  id_cols=row,
  values_fn=function(x) x[1]
)
# remove the `row` column
nodes_df <- nodes_df[, columns]
# convert `Id` column to integer to match edges data frame
nodes_df$Id <- as.integer(nodes_df$Id)


# take a look
head(nodes_df)
# the node data provides the index of the nodes
# as well as other attributes

head(edges)
# edges data include the indices of two nodes
# indicating the two nodes that form an edge

# construct the graph using graph.data.frame
# this function constructs a graph by feeding
# in the edges and nodes data frames. The first
# two columns of the edge data should be the IDs
# of the two endpoints of each edge, and the
# first column of the node data should be the
# ID of that node.
# We set directed=F for undirected graphs
G_full = graph_from_data_frame(edges, directed=F, nodes_df)

# prune nodes with degree less than `deg_threshold` so that
# graphs can be rendered in a reasonable amount of time
deg_threshold <- 40
G_degrees <- degree(G_full, mode = "all")
G_orphans <- which(G_degrees < deg_threshold)
G <- delete_vertices(G_full, G_orphans)

# plotting graphs
# plotting a graph is separated into two steps:
# - layout (there are many ways to do this)
# - plotting (actually draws it)
# we set the seed so the layout is the same
set.seed(45)
mylayout = layout_nicely(G)
# we will use the same layout "mylayout" throughout
# try just plain plot function
plot(G, layout=mylayout)
# quite ugly!
# First, lets fix the margins (which are
# usually fine by default with ggplot)
par(mar=c(0, 0, 0, 0))
plot(G, layout=mylayout)

# Still a mess - let's try to make it nicer
# Remove the node labels, and let the 
# size vary as the square root of degree 
plot(G, layout=mylayout, vertex.label=NA, vertex.size=2*sqrt(degree(G)))
# a little nicer!

# we can make the color vary too, but 
# it is a little trickier. If this doesn't make
# much sense, don't worry too much!
### ALERT: EXPERT ONLY ####
# get.colors is a function that we write -- it takes
# as input two colors (the color for low values and
# the color for high values) as well as values and it
# returns the color for each value.
get.colors = function(cols, x) {
  ramp = colorRamp(cols)
  apply(ramp(x/max(x)), 1, function(y) rgb(y[1]/255, y[2]/255, y[3]/255))
}

# You don't need to understand or change get.colors
# to use it when plotting:
plot(G, layout=mylayout, vertex.label=NA, vertex.size=2*sqrt(degree(G)),
     vertex.color = get.colors(c("white", "purple"), degree(G)^2))
# 2^degree(G) emphasizes the nodes with the highest degrees
### END ALERT ###


### PART II:      Network metrics           ###### 
# From now on, we are only going to work with
# students who are in the largest connected
# component of the graph

# we use `G` (instead of `G_full`) for better performance in plotting,
# but `G_full` when computing actual degree centrality
comp = components(G)
# inspect the membership
comp$membership
table(comp$membership)
# only keep those that are in the largest
in.max.comp = (comp$membership == 1)
sg = induced_subgraph(G, in.max.comp)
# get the sub layout
sg_mylayout = mylayout[in.max.comp,]
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=2*sqrt(degree(sg)))

V(sg)[degree(sg) == max(degree(sg))]

# Metric 1: Degree Centrality
G_full.degree <- data.frame(deg=degree(G_full))
G_full.degree$Id <- row.names(G_full.degree)
# sort by degree in decreasing order to obtain the top 100 highest degrees
G_full.degree <- G_full.degree[order(G_full.degree$deg, decreasing = T),]
G_full.degree.100 <- G_full.degree[1:100,]
# merge with the metadata table to identify each node
G_full.degree.100 <- merge(G_full.degree.100, nodes_df, by="Id")
# sort by descending degree again after merging
G_full.degree.100 <- G_full.degree.100[
  order(G_full.degree.100$deg, decreasing = T),
]
# export as csv
write.csv(G_full.degree.100, "G_full.degree.100.csv", row.names = F)
# We can plot by degree (which we already did!)
# The eye measures the size of a point by its
# area but vertex.size scales the radius of the
# point, so you almost always want to apply the
# square root ("sqrt" in R) to any value that you
# want to represent with the size of nodes. You
# should multiply the square root by whatever
# value is needed to make the points visually
# appealing.
plot(G, layout=mylayout, vertex.label=NA,
     vertex.size=2*sqrt(degree(G)),
     vertex.color = get.colors(c("white", "purple"), degree(G)))

# Metric 2: Closeness Centrality
# How "central" a node is in a network
# = 1 / sum(shortest path lengths to other nodes)
# Recall that closeness only works on connected components,
# so we find the largest connected component in `G_full`
comp_full = components(G_full)
comp_full$membership
# only keep those that are in the largest
in.max.comp_full = (comp_full$membership == 1)
G_conn = induced_subgraph(G_full, in.max.comp_full)

G_conn_cl = closeness(G_conn, cutoff = 3)
summary(G_conn_cl)

G_sub.closeness <- data.frame(cls=G_conn_cl)
G_sub.closeness$Id <- row.names(G_sub.closeness)
G_sub.closeness <- G_sub.closeness[order(G_sub.closeness$cls, decreasing = T),]
G_sub.closeness.100 <- G_sub.closeness[1:100,]
G_sub.closeness.100 <- merge(G_sub.closeness.100, nodes_df, by="Id")
G_sub.closeness.100 <- G_sub.closeness.100[
  order(G_sub.closeness.100$cls, decreasing = T),
]
# export as csv
write.csv(G_sub.closeness.100, "G_sub.closeness.100.csv", row.names = F)
# Basic plot
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=10000*(sg_cl))
# Or with colors!
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=200*sqrt(sg_cl), 
     vertex.color = get.colors(c("white", "purple"), sg_cl^10))

# Metric 3: Betweenness Centrality
# Number of shortest paths in the network in
# which this node appears
bn = betweenness(G)
summary(bn)

G_sub.between <- data.frame(btw=betweenness(G))
G_sub.between$Id <- row.names(G_sub.between)
G_sub.between <- G_sub.between[order(G_sub.between$btw, decreasing = T),]
G_sub.between.100 <- G_sub.between[1:100,]
G_sub.between.100 <- merge(G_sub.between.100, nodes_df, by="Id")
G_sub.between.100 <- G_sub.between.100[
  order(G_sub.between.100$btw, decreasing = T),
]
# export as csv
write.csv(G_sub.between.100, "G_sub.between.100.csv", row.names = F)
# Try basic plot:
plot(G, layout=mylayout, vertex.label=NA, vertex.size=sqrt(bn))
# Not good! We need to shrink them a bit
plot(G, layout=mylayout, vertex.label=NA, vertex.size=0.2*sqrt(bn))
# Really shows the range, but is hard to look at. Try sqrt + color
plot(sg, layout=mylayout, vertex.label=NA, vertex.size=0.2*sqrt(bn),
     vertex.color = get.colors(c("white", "purple"), bn))

# Metric 4: PageRank
# The page.rank function returns more than one thing,
# but what we want is the "$vector" after calling page.rank
pr = page_rank(G)$vector
summary(pr)
# Plot it
plot(G, layout=mylayout, vertex.label=NA, vertex.size=100*sqrt(pr),
     vertex.color = get.colors(c("white", "purple"), pr^2))

pr_full = page_rank(G_full)$vector
summary(pr_full)
G_full.pagerank <- data.frame(pr=pr_full)
G_full.pagerank$Id <- row.names(G_full.pagerank)
G_full.pagerank <- G_full.pagerank[order(G_full.pagerank$pr, decreasing = T),]
G_full.pagerank.100 <- G_full.pagerank[1:100,]
G_full.pagerank.100 <- merge(G_full.pagerank.100, nodes_df, by="Id")
G_full.pagerank.100 <- G_full.pagerank.100[
  order(G_full.pagerank.100$pr, decreasing = T),
]

# export as csv
write.csv(G_full.pagerank.100, "G_full.pagerank.100.csv", row.names = F)

###     PART III: Community Detection     #####
# igraph makes this pretty easy too
# There are a few ways to cluster communities, and
# different algorithms - its a hard problem, so some
# methods are fast and less accurate, some are more
# We'll use the cluster_spinglass function for this graph
# (due to how the algorithm works, cluster_spinglass
#  only works if we have a single connected component;
#  if you have multiple components you might try
#  cluster_fast_greedy instead)
set.seed(144)
clust = cluster_spinglass(sg, spins = 100)$membership
# Now we have a cluster for each node (if you set a
# different random seed you may have different clusters)
clust
table(clust)

# Plot the clusters. We're doing some magic here
# to try to give everything a mix of shapes and colors
# You don't need to understand the next lines
# Here is the version that was used in class for
# the full graph, where we had more communities
# color = rep(brewer.pal(12, "Paired"), 8)
# shape = rep(rep(c("circle", "square", "csquare", "sphere"), each=12), 2)
# plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=3,
#      vertex.color=color[clust],
#      vertex.shape=shape[clust])
# We don't have that many, so we can go simpler
color = c(brewer.pal(12, "Paired"), "black")
plot(sg, layout=sg_mylayout, vertex.label=NA, vertex.size=0.2*sqrt(bn),
     vertex.color=color[clust])

