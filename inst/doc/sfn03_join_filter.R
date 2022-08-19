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

## ---- fig.show='hold', out.width = '50%'--------------------------------------
p1 = st_point(c(4151358, 3208045))
p2 = st_point(c(4151340, 3207120))
p3 = st_point(c(4151856, 3207106))
p4 = st_point(c(4151874, 3208031))

poly = st_multipoint(c(p1, p2, p3, p4)) %>%
  st_cast("POLYGON") %>%
  st_sfc(crs = 3035)

net = as_sfnetwork(roxel) %>%
  st_transform(3035)

filtered = st_filter(net, poly, .pred = st_intersects)

plot(net, col = "grey")
plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(net, col = "grey")
plot(filtered, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
filtered = net %>%
  activate("edges") %>%
  st_filter(poly, .pred = st_intersects)

plot(net, col = "grey")
plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(net, col = "grey")
plot(filtered, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
filtered = net %>%
  activate("edges") %>%
  st_filter(poly, .pred = st_intersects) %>%
  activate("nodes") %>%
  filter(!node_is_isolated())

plot(net, col = "grey")
plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(net, col = "grey")
plot(filtered, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
point = st_centroid(st_combine(net))

filtered = net %>%
  activate("nodes") %>%
  st_filter(point, .predicate = st_is_within_distance, dist = 500)

plot(net, col = "grey")
plot(point, col = "red", cex = 3, pch = 20, add = TRUE)
plot(net, col = "grey")
plot(filtered, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
filtered = net %>%
  activate("edges") %>%
  filter(edge_intersects(poly)) %>%
  activate("nodes") %>%
  filter(!node_is_isolated())

plot(net, col = "grey")
plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(net, col = "grey")
plot(filtered, add = TRUE)

## -----------------------------------------------------------------------------
net %>%
  activate("edges") %>%
  filter(edge_crosses(.E()))

## -----------------------------------------------------------------------------
net %>%
  mutate(in_poly = node_intersects(poly))

## ---- fig.show='hold', out.width = '50%'--------------------------------------
v = 4152000
l = st_linestring(rbind(c(v, st_bbox(net)["ymin"]), c(v, st_bbox(net)["ymax"])))

filtered_by_coords = net %>%
  activate("nodes") %>%
  filter(node_X() > v)

plot(net, col = "grey")
plot(l, col = "red", lty = 4, lwd = 4, add = TRUE)
plot(net, col = "grey")
plot(filtered_by_coords, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
clipped = net %>%
  activate("edges") %>%
  st_intersection(poly) %>%
  activate("nodes") %>%
  filter(!node_is_isolated())

plot(net, col = "grey")
plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(net, col = "grey")
plot(clipped, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
codes = net %>%
  st_make_grid(n = c(2, 2)) %>%
  st_as_sf() %>%
  mutate(post_code = as.character(seq(1000, 1000 + n() * 10 - 10, 10)))

joined = st_join(net, codes, join = st_intersects)
joined
plot(net, col = "grey")
plot(codes, col = NA, border = "red", lty = 4, lwd = 4, add = TRUE)
text(st_coordinates(st_centroid(st_geometry(codes))), codes$post_code, cex = 2)
plot(st_geometry(joined, "edges"))
plot(st_as_sf(joined, "nodes"), pch = 20, add = TRUE)

## -----------------------------------------------------------------------------
two_equal_polys = st_as_sf(c(poly, poly)) %>%
  mutate(foo = c("a", "b"))

# Join on nodes gives a warning that only the first match per node is joined.
# The number of nodes in the resulting network remains the same.
st_join(net, two_equal_polys, join = st_intersects)
# Join on edges duplicates edges that have multiple matches.
# The number of edges in the resulting network is higher than in the original.
net %>%
  activate("edges") %>%
  st_join(two_equal_polys, join = st_intersects)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
# Create a network.
node1 = st_point(c(0, 0))
node2 = st_point(c(1, 0))
edge = st_sfc(st_linestring(c(node1, node2)))

net = as_sfnetwork(edge)

# Create a set of POIs.
pois = data.frame(poi_type = c("bakery", "butcher"),
                  x = c(0, 0.6), y = c(0.2, 0.2)) %>%
  st_as_sf(coords = c("x", "y"))

# Find indices of nearest nodes.
nearest_nodes = st_nearest_feature(pois, net)

# Snap geometries of POIs to the network.
snapped_pois = pois %>%
  st_set_geometry(st_geometry(net)[nearest_nodes])

# Plot.
plot_connections = function(pois) {
  for (i in seq_len(nrow(pois))) {
    connection = st_nearest_points(pois[i, ], net)[nearest_nodes[i]]
    plot(connection, col = "grey", lty = 2, lwd = 2, add = TRUE)
  }
}

plot(net, cex = 2, lwd = 4)
plot_connections(pois)
plot(pois, pch = 8, cex = 2, lwd = 2, add = TRUE)
plot(net, cex = 2, lwd = 4)
plot(snapped_pois, pch = 8, cex = 2, lwd = 2, add = TRUE)

## -----------------------------------------------------------------------------
st_join(net, snapped_pois)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
blended = st_network_blend(net, pois)
blended
plot_connections = function(pois) {
  for (i in seq_len(nrow(pois))) {
    connection = st_nearest_points(pois[i, ], activate(net, "edges"))
    plot(connection, col = "grey", lty = 2, lwd = 2, add = TRUE)
  }
}

plot(net, cex = 2, lwd = 4)
plot_connections(pois)
plot(pois, pch = 8, cex = 2, lwd = 2, add = TRUE)
plot(blended, cex = 2, lwd = 4)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
pois = data.frame(poi_type = c("bakery", "butcher", "bar"),
                  x = c(0, 0.6, 0.4), y = c(0.2, 0.2, 0.3)) %>%
  st_as_sf(coords = c("x", "y"))

blended = st_network_blend(net, pois)
blended_with_tolerance = st_network_blend(net, pois, tolerance = 0.2)

plot(blended, cex = 2, lwd = 4)
plot_connections(pois)
plot(pois, pch = 8, cex = 2, lwd = 2, add = TRUE)
plot(blended_with_tolerance, cex = 2, lwd = 4)
plot_connections(pois)
plot(pois, pch = 8, cex = 2, lwd = 2, add = TRUE)

## -----------------------------------------------------------------------------
# Create two intersecting lines.
p1 = st_point(c(0.53236, 1.95377))
p2 = st_point(c(0.53209, 1.95328))
l1 = st_sfc(st_linestring(c(p1, p2)))

p3 = st_point(c(0.53209, 1.95345))
p4 = st_point(c(0.53245, 1.95345))
l2 = st_sfc(st_linestring(c(p3, p4)))

# The two lines share an intersection point.
st_intersection(l1, l2)

# But this intersection point does not intersects the line itself!
st_intersects(l1, st_intersection(l1, l2), sparse = FALSE)

# The intersection point is instead located a tiny bit next to the line.
st_distance(l1, st_intersection(l1, l2))

## ---- fig.show='hold', out.width = '50%'--------------------------------------
net = as_sfnetwork(l1)
p = st_intersection(l1, l2)

plot(l1)
plot(l2, col = "grey", lwd = 2, add = TRUE)
plot(st_network_blend(net, p, tolerance = 0), lwd = 2, cex = 2, add = TRUE)
plot(l1)
plot(l2, col = "grey", lwd = 2, add = TRUE)
plot(st_network_blend(net, p, tolerance = 1e-10), lwd = 2, cex = 2, add = TRUE)

## ---- fig.show='hold', out.width = '50%'--------------------------------------
node3 = st_point(c(1, 1))
node4 = st_point(c(0, 1))
edge2 = st_sfc(st_linestring(c(node2, node3)))
edge3 = st_sfc(st_linestring(c(node3, node4)))

net = as_sfnetwork(c(edge, edge2))
other_net = as_sfnetwork(c(edge2, edge3))

joined = st_network_join(net, other_net)
joined
plot(net, pch = 15, cex = 2, lwd = 4)
plot(other_net, col = "red", pch = 18, cex = 2, lty = 2, lwd = 4, add = TRUE)
plot(joined, cex = 2, lwd = 4)

## ---- include = FALSE---------------------------------------------------------
par(oldpar)
options(oldoptions)

