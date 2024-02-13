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
# ideally we would use the largest connected component in `G_full`,
# but `G` is used instead for better performance
# when running the `closeness` function
G_conn = induced_subgraph(G, components(G)$membership == 1)
G_conn_layout = layout_nicely(G_conn)
G_conn_cl = closeness(G_conn)
summary(G_conn_cl)

G_conn.closeness <- data.frame(cls=G_conn_cl)
G_conn.closeness$Id <- row.names(G_conn.closeness)
# sort by decreasing closeness to get the top 100
G_conn.closeness <- G_conn.closeness[order(
  G_conn.closeness$cls,
  decreasing = T
),]
G_conn.closeness.100 <- G_conn.closeness[1:100,]
# merge with node metadata and sort again
G_conn.closeness.100 <- merge(G_conn.closeness.100, nodes_df, by="Id")
G_conn.closeness.100 <- G_conn.closeness.100[
  order(G_conn.closeness.100$cls, decreasing = T),
]
# export as csv
write.csv(G_conn.closeness.100, "G_conn.closeness.100.csv", row.names = F)
# Basic plot
plot(G_conn, layout=G_conn_layout,
     vertex.label=NA, vertex.size=10000*(G_conn_cl))
# Or with colors!
plot(G_conn, layout=G_conn_layout, vertex.label=NA,
     vertex.size=200*sqrt(G_conn_cl), 
     vertex.color = get.colors(c("white", "purple"), G_conn_cl^10))

# Metric 3: Betweenness Centrality
# Number of shortest paths in the network in
# which this node appears
# Again, `G` is used instead of `G_full` for
# faster computation over accuracy
bn = betweenness(G)
summary(bn)

G.between <- data.frame(btw=betweenness(G))
G.between$Id <- row.names(G.between)
# sort by decreasing betweenness to obtain the top 100
G.between <- G.between[order(G.between$btw, decreasing = T),]
G.between.100 <- G.between[1:100,]
G.between.100 <- merge(G.between.100, nodes_df, by="Id")
G.between.100 <- G.between.100[
  order(G.between.100$btw, decreasing = T),
]
# export as csv
write.csv(G.between.100, "G.between.100.csv", row.names = F)
# Try basic plot:
plot(G, layout=mylayout, vertex.label=NA, vertex.size=sqrt(bn))
# Not good! We need to shrink them a bit
plot(G, layout=mylayout, vertex.label=NA, vertex.size=0.2*sqrt(bn))
# Really shows the range, but is hard to look at. Try sqrt + color
plot(G, layout=mylayout, vertex.label=NA, vertex.size=0.2*sqrt(bn),
     vertex.color = get.colors(c("white", "purple"), bn))

# Metric 4: PageRank
# The page.rank function returns more than one thing,
# but what we want is the "$vector" after calling page.rank
# We use `G` for better performance while plotting,
# but `G_full` to compute the top 100 nodes by PageRank
pr = page_rank(G)$vector
# Plot it
plot(G, layout=mylayout, vertex.label=NA, vertex.size=100*sqrt(pr),
     vertex.color = get.colors(c("white", "purple"), pr^2))

pr_full = page_rank(G_full)$vector
summary(pr_full)
G_full.pagerank <- data.frame(pr=pr_full)
G_full.pagerank$Id <- row.names(G_full.pagerank)
# sort by descending PageRank to obtain the top 100
G_full.pagerank <- G_full.pagerank[order(G_full.pagerank$pr, decreasing = T),]
G_full.pagerank.100 <- G_full.pagerank[1:100,]
# merge with the nodes metadata and sort again
G_full.pagerank.100 <- merge(G_full.pagerank.100, nodes_df, by="Id")
G_full.pagerank.100 <- G_full.pagerank.100[
  order(G_full.pagerank.100$pr, decreasing = T),
]

# export as csv
write.csv(G_full.pagerank.100, "G_full.pagerank.100.csv", row.names = F)
