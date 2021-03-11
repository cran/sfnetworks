## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(global.par = TRUE)

## ----plot, echo=FALSE, results='asis'-----------------------------------------
# plot margins
oldpar = par(no.readonly = TRUE)
par(mar = c(1, 1, 1, 1))
# crayon needs to be explicitly activated in Rmd
oldoptions = options()
options(crayon.enabled = TRUE)
# Hooks needs to be set to deal with outputs
# thanks to fansi logic
old_hooks = fansi::set_knit_hooks(
  knitr::knit_hooks,
  which = c("output", "message", "error")
)

## ---- message=FALSE-----------------------------------------------------------
library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
library(igraph)
library(dbscan)

## -----------------------------------------------------------------------------
p1 = st_point(c(7, 51))
p2 = st_point(c(8, 52))
p3 = st_point(c(8.000001, 52.000001))
p4 = st_point(c(7, 52))

l1 = st_sfc(st_linestring(c(p1, p2)))
l2 = st_sfc(st_linestring(c(p3, p4)))

edges = st_as_sf(c(l1, l2), crs = 4326)

# The edges are not connected.
as_sfnetwork(edges)

## -----------------------------------------------------------------------------
# Round coordinates to 0 digits.
st_geometry(edges) = st_geometry(edges) %>%
  lapply(function(x) round(x, 0)) %>%
  st_sfc(crs = st_crs(edges))

# The edges are connected.
as_sfnetwork(edges)

## -----------------------------------------------------------------------------
p1 = st_point(c(7, 51))
p2 = st_point(c(7, 52))
p3 = st_point(c(8, 52))

l1 = st_sfc(st_linestring(c(p1, p2)))
l2 = st_sfc(st_linestring(c(p1, p3)))
l3 = st_sfc(st_linestring(c(p3, p2)))

edges = st_as_sf(c(l1, l2, l3), crs = 4326)
edges$oneway = c(TRUE, FALSE, FALSE)
edges

## -----------------------------------------------------------------------------
duplicates = edges[!edges$oneway, ]
reversed_duplicates = st_reverse(duplicates)

edges = rbind(edges, reversed_duplicates)
net = as_sfnetwork(edges)
activate(net, "edges")

## ---- fig.height=5, fig.width=5-----------------------------------------------
p1 = st_point(c(0, 1))
p2 = st_point(c(1, 1))
p3 = st_point(c(2, 1))
p4 = st_point(c(3, 1))
p5 = st_point(c(4, 1))
p6 = st_point(c(3, 2))
p7 = st_point(c(3, 0))
p8 = st_point(c(4, 3))
p9 = st_point(c(4, 2))
p10 = st_point(c(4, 0))
p11 = st_point(c(5, 2))
p12 = st_point(c(5, 0))
p13 = st_point(c(5, -1))
p14 = st_point(c(5.8, 1))
p15 = st_point(c(6, 1.2))
p16 = st_point(c(6.2, 1))
p17 = st_point(c(6, 0.8))
p18 = st_point(c(6, 2))
p19 = st_point(c(6, -1))
p20 = st_point(c(7, 1))

l1 = st_sfc(st_linestring(c(p1, p2, p3)))
l2 = st_sfc(st_linestring(c(p3, p4, p5)))
l3 = st_sfc(st_linestring(c(p6, p4, p7)))
l4 = st_sfc(st_linestring(c(p8, p11, p9)))
l5 = st_sfc(st_linestring(c(p9, p5, p10)))
l6 = st_sfc(st_linestring(c(p8, p9)))
l7 = st_sfc(st_linestring(c(p10, p12, p13, p10)))
l8 = st_sfc(st_linestring(c(p5, p14)))
l9 = st_sfc(st_linestring(c(p15, p14)))
l10 = st_sfc(st_linestring(c(p16, p15)))
l11 = st_sfc(st_linestring(c(p14, p17)))
l12 = st_sfc(st_linestring(c(p17, p16)))
l13 = st_sfc(st_linestring(c(p15, p18)))
l14 = st_sfc(st_linestring(c(p17, p19)))
l15 = st_sfc(st_linestring(c(p16, p20)))

lines = c(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15)

edge_colors = function(x) rep(sf.colors(12, categorical = TRUE)[-2], 2)[c(1:ecount(x))]

net = as_sfnetwork(lines)
plot(st_geometry(net, "edges"), col = edge_colors(net), lwd = 4)
plot(st_geometry(net, "nodes"), pch = 20, cex = 2, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
simple = net %>%
  activate("edges") %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())

plot(st_geometry(net, "edges"), col = edge_colors(net), lwd = 4)
plot(st_geometry(net, "nodes"), pch = 20, cex = 1.5, add = TRUE)
plot(st_geometry(simple, "edges"), col = edge_colors(simple), lwd = 4)
plot(st_geometry(simple, "nodes"), pch = 20, cex = 1.5, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
simple = net %>%
  activate("edges") %>%
  arrange(edge_length()) %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())

plot(st_geometry(net, "edges"), col = edge_colors(net), lwd = 4)
plot(st_geometry(net, "nodes"), pch = 20, cex = 1.5, add = TRUE)
plot(st_geometry(simple, "edges"), col = edge_colors(simple), lwd = 4)
plot(st_geometry(simple, "nodes"), pch = 20, cex = 1.5, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
# Add some attribute columns to the edges table.
flows = sample(1:10, ecount(net), replace = TRUE)
types = sample(c("path", "road"), ecount(net), replace = TRUE)
foo = sample(c(1:ecount(net)), ecount(net))
bar = sample(letters, ecount(net))

net = net %>%
  activate("edges") %>%
  arrange(edge_length()) %>%
  mutate(flow = flows, type = types, foo = foo, bar = bar)

net
# We know from before that our example network has one set of multiple edges.
# Lets look at them.
net %>%
  activate("edges") %>%
  filter(edge_is_between(6, 7)) %>%
  st_as_sf()
# Define how we want to combine the attributes.
# For the flows:
# --> It makes sense to sum them when edges get merged.
# For the type:
# --> Preserve the type only if all edges in a set have the same type.
# For all other attributes:
# --> Drop these attributes.
combinations = list(
  flow = "sum",
  type = function(x) if (length(unique(x)) == 1) x[1] else NA,
  "ignore"
)

# Simplify the network with to_spatial_simple.
simple = convert(net, to_spatial_simple, summarise_attributes = combinations)

# Inspect our merged set of multiple edges.
simple %>%
  activate("edges") %>%
  filter(edge_is_between(6, 7)) %>%
  st_as_sf()
plot(st_geometry(net, "edges"), col = edge_colors(net), lwd = 4)
plot(st_geometry(net, "nodes"), pch = 20, cex = 1.5, add = TRUE)
plot(st_geometry(simple, "edges"), col = edge_colors(simple), lwd = 4)
plot(st_geometry(simple, "nodes"), pch = 20, cex = 1.5, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
subdivision = convert(simple, to_spatial_subdivision)

plot(st_geometry(simple, "edges"), col = edge_colors(simple), lwd = 4)
plot(st_geometry(simple, "nodes"), pch = 20, cex = 1.5, add = TRUE)
plot(st_geometry(subdivision, "edges"), col = edge_colors(subdivision), lwd = 4)
plot(st_geometry(subdivision, "nodes"), pch = 20, cex = 1.5, add = TRUE)

## ---- message=FALSE, fig.show='hold', out.width = '50%'-----------------------
smoothed = convert(subdivision, to_spatial_smooth)

plot(st_geometry(subdivision, "edges"), col = edge_colors(subdivision), lwd = 4)
plot(st_geometry(subdivision, "nodes"), pch = 20, cex = 1.5, add = TRUE)
plot(st_geometry(smoothed, "edges"), col = edge_colors(smoothed), lwd = 4)
plot(st_geometry(smoothed, "nodes"), pch = 20, cex = 1.5, add = TRUE)

## -----------------------------------------------------------------------------
# Retrieve the coordinates of the nodes.
node_coords = smoothed %>%
  activate("nodes") %>%
  st_coordinates()

# Cluster the nodes with the DBSCAN spatial clustering algorithm.
# We set eps = 0.5 such that:
# Nodes within a distance of 0.5 from each other will be in the same cluster.
# We set minPts = 1 such that:
# A node is assigned a cluster even if it is the only member of that cluster.
clusters = dbscan(node_coords, eps = 0.5, minPts = 1)$cluster

# Add the cluster information to the nodes of the network.
clustered = smoothed %>%
  activate("nodes") %>%
  mutate(cls = clusters)

## -----------------------------------------------------------------------------
clustered = clustered %>%
  mutate(cmp = group_components())

select(clustered, cls, cmp)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
contracted = convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE
)

plot(st_geometry(smoothed, "edges"), col = edge_colors(smoothed), lwd = 4)
plot(st_geometry(smoothed, "nodes"), pch = 20, cex = 1.5, add = TRUE)
plot(st_geometry(contracted, "edges"), col = edge_colors(contracted), lwd = 4)
plot(st_geometry(contracted, "nodes"), pch = 20, cex = 1.5, add = TRUE)

## -----------------------------------------------------------------------------
# Add some additional attribute columns to the nodes table.
clustered = clustered %>%
  activate("nodes") %>%
  mutate(is_priority = sample(
    c(TRUE, FALSE),
    vcount(clustered),
    replace = TRUE
  ))

# We know from before there is one group with several close, connected nodes.
# Lets look at them.
clustered %>%
  activate("nodes") %>%
  filter(cls == 4 & cmp == 1) %>%
  st_as_sf()
# Define how we want to combine the attributes.
# For the boolean is_priority variable:
# --> We want it to be TRUE if any of the nodes has a values of TRUE.
# For the others, which were used as grouping variables:
# --> Drop these attributes.
combinations = list(
  is_priority = function(x) any(x),
  "ignore"
)

# Contract with to_spatial_contracted.
contracted = convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE,
  summarise_attributes = combinations
)

# Inspect our contracted group of nodes.
contracted %>%
  activate("nodes") %>%
  slice(4) %>%
  st_as_sf()

## ---- fig.show='hold', out.width = '50%'--------------------------------------
plot(st_geometry(net, "edges"), col = edge_colors(net), lwd = 4)
plot(st_geometry(net, "nodes"), pch = 20, cex = 1.5, add = TRUE)
plot(st_geometry(contracted, "edges"), col = edge_colors(contracted), lwd = 4)
plot(st_geometry(contracted, "nodes"), pch = 20, cex = 1.5, add = TRUE)

## ---- include = FALSE---------------------------------------------------------
par(oldpar)
options(oldoptions)

