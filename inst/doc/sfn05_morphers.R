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

## -----------------------------------------------------------------------------
net = as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035)

grouped_net = net %>%
  morph(to_linegraph) %>%
  mutate(group = group_louvain()) %>%
  unmorph()

grouped_net
# The algorithm detected 34 communities.
grouped_net %>%
  activate("edges") %>%
  pull(group) %>%
  unique() %>%
  length()

## ---- fig.width=5, fig.height=5-----------------------------------------------
plot(st_geometry(net, "edges"), col = "grey", lwd = 0.5)

grouped_net %>%
  activate("edges") %>%
  st_as_sf() %>%
  transmute(group = as.factor(group)) %>%
  filter(group %in% c(1:11)) %>%
  plot(lwd = 4, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
new_net = net %>%
  mutate(is_cut = node_is_cut()) %>%
  morph(to_linegraph) %>%
  mutate(is_cut = node_is_cut()) %>%
  unmorph()

cut_nodes = new_net %>%
  activate("nodes") %>%
  filter(is_cut) %>%
  st_geometry()

cut_edges = new_net %>%
  activate("edges") %>%
  filter(is_cut) %>%
  st_geometry()

plot(net, col = "grey", main = "Cut nodes")
plot(cut_nodes, col = "red", pch = 20, cex = 2, add = TRUE)
plot(net, col = "grey", main = "Cut edges")
plot(cut_edges, col = "red", lwd = 4, add = TRUE)

## -----------------------------------------------------------------------------
morphed_net = morph(net, to_components)

morphed_net
class(morphed_net)

length(morphed_net)

## -----------------------------------------------------------------------------
convert(net, to_complement)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
new_net = net %>%
  activate("nodes") %>%
  filter(group_components() == 1) %>%
  mutate(foo = sample(c(1:10), graph_order(), replace = TRUE)) %>%
  mutate(bar = sample(c(TRUE, FALSE), graph_order(), replace = TRUE)) %>%
  mutate(louvain = as.factor(group_louvain()))

contracted_net = convert(
  new_net,
  to_spatial_contracted,
  louvain,
  simplify = TRUE,
  summarise_attributes = list(
    foo = "sum",
    bar = function(x) any(x),
    louvain = "first"
  )
)

plot(st_geometry(new_net, "edges"), main = "Grouped nodes")
plot(st_as_sf(new_net)["louvain"], key.pos = NULL, pch = 20, add = TRUE)
plot(st_geometry(contracted_net, "edges"), main = "Contracted network")
plot(
  st_as_sf(contracted_net)["louvain"],
  cex = 2, key.pos = NULL,
  pch = 20, add = TRUE
)

## -----------------------------------------------------------------------------
net %>%
  activate("nodes") %>%
  mutate(bc_undir = centrality_betweenness()) %>%
  morph(to_spatial_directed) %>%
  mutate(bc_dir = centrality_betweenness()) %>%
  unmorph() %>%
  mutate(bc_diff = bc_dir - bc_undir) %>%
  arrange(bc_diff, desc())

## ---- fig.show='hold', out.width = '50%'--------------------------------------
implicit_net = st_set_geometry(activate(net, "edges"), NULL)
explicit_net = convert(implicit_net, to_spatial_explicit)

plot(implicit_net, draw_lines = FALSE, main = "Implicit edges")
plot(explicit_net, main = "Explicit edges")

## ---- fig.width=5, fig.height=5-----------------------------------------------
# As an example we will calculate multiple neighborhoods with different thresholds.
# First we set the geographic lengths of the edges as the edge weights.
# These weights will automatically be used when calculating travel costs.
# Just as in the shortest paths calculation functions.
new_net = net %>%
  activate("edges") %>%
  mutate(weight = edge_length())

# Define the origin location.
p = net %>%
  st_geometry() %>%
  st_combine() %>%
  st_centroid()

# Define the threshold values (in meters).
# Define also the colors to plot the neighborhoods in.
thresholds = rev(seq(100, 1000, 100))
palette = sf.colors(n = 10)

# Plot the results.
plot(net, col = "grey")

for (i in c(1:10)) {
  nbh = convert(net, to_spatial_neighborhood, p, thresholds[i])
  plot(nbh, col = palette[i], add = TRUE)
}

plot(p, pch = 8, cex = 2, lwd = 2, add = TRUE)

## -----------------------------------------------------------------------------
net %>%
  activate("edges") %>%
  convert(
    to_spatial_shortest_paths,
    from = 1, to = 100,
    weights = edge_length()
  )

## ---- fig.width=5, fig.height=5-----------------------------------------------
new_net = net %>%
  activate("edges") %>%
  morph(
    to_spatial_shortest_paths,
    from = 1, to = seq(10, 100, 10),
    weights = edge_length()
  ) %>%
  mutate(in_paths = TRUE) %>%
  unmorph()

new_net %>%
  st_geometry() %>%
  plot(col = "grey", lwd = 2)

new_net %>%
  filter(in_paths) %>%
  st_geometry() %>%
  plot(col = "red", lwd = 4, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
# Add a flow attribute to the edges.
# When merging multiple edges, we want the flow of the new edge to be:
# --> The sum of the flows of the merged edges.
new_net = net %>%
  activate("edges") %>%
  mutate(flow = sample(c(1:100), ecount(net), replace = TRUE))

# Select a set of multiple edges to inspect before simplifying.
a_multiple = new_net %>%
  filter(edge_is_multiple()) %>%
  slice(1)

new_net %>%
  filter(edge_is_between(pull(a_multiple, from), pull(a_multiple, to))) %>%
  st_as_sf()
# Simplify the network.
# We summarise the flow attribute by taking the sum of the merged edge flows.
# For all the other attributes we simply take the first value in the set.
simple_net = new_net %>%
  convert(
    to_spatial_simple,
    summarise_attributes = list(flow = "sum", "first")
  )

# The multiple edges are merged into one.
# The flow is summarised by taking the sum of the merged edge flows.
simple_net %>%
  filter(edge_is_between(pull(a_multiple, from), pull(a_multiple, to))) %>%
  st_as_sf()

## ---- fig.show='hold', out.width = '50%'--------------------------------------
smoothed_net = convert(net, to_spatial_smooth)

plot(net, main = "Original network")
plot(net, col = "red", cex = 0.8, lwd = 0.1, main = "Smoothed network")
plot(smoothed_net, col = "grey", add = TRUE)

## -----------------------------------------------------------------------------
subdivided_net = convert(net, to_spatial_subdivision)

# Original network.
paste("Number of edges: ", ecount(net))
paste("Number of components: ", count_components(net))

# Subdivided network.
# The whole network is now a single connected component!
paste("Number of edges: ", ecount(subdivided_net))
paste("Number of components: ", count_components(subdivided_net))

## -----------------------------------------------------------------------------
codes = net %>%
  st_make_grid(n = c(2, 2)) %>%
  st_as_sf() %>%
  mutate(post_code = seq(1000, 1000 + n() * 10 - 10, 10))

points = st_geometry(net, "nodes")[c(2, 3)]

net %>%
  morph(to_spatial_subset, points, .pred = st_equals) %>%
  st_join(codes, join = st_intersects) %>%
  unmorph()

## -----------------------------------------------------------------------------
net = net %>%
  activate("nodes") %>%
  mutate(building = sample(c(TRUE, FALSE), n(), replace = TRUE))

net %>%
  morph(to_subgraph, building) %>%
  st_join(codes, join = st_intersects) %>%
  unmorph()

## ---- error = TRUE------------------------------------------------------------
# Azimuth calculation fails with our projected CRS.
# The function complains the coordinates are not longitude/latitude.
net %>%
  activate("edges") %>%
  mutate(azimuth = edge_azimuth())

## -----------------------------------------------------------------------------
# We make it work by temporarily transforming to a different CRS.
net %>%
  activate("edges") %>%
  morph(to_spatial_transformed, 4326) %>%
  mutate(azimuth = edge_azimuth()) %>%
  unmorph()

## ---- include = FALSE---------------------------------------------------------
par(oldpar)
options(oldoptions)

