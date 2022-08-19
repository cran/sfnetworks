## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(global.par = TRUE)
current_geos = numeric_version(sf::sf_extSoftVersion()["GEOS"])
required_geos = numeric_version("3.7.0")
geos37 = current_geos >= required_geos

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
p1 = st_point(c(7, 51))
p2 = st_point(c(7, 52))
p3 = st_point(c(8, 52))
p4 = st_point(c(8, 51.5))

l1 = st_sfc(st_linestring(c(p1, p2)))
l2 = st_sfc(st_linestring(c(p1, p4, p3)))
l3 = st_sfc(st_linestring(c(p3, p2)))

edges = st_as_sf(c(l1, l2, l3), crs = 4326)
nodes = st_as_sf(c(st_sfc(p1), st_sfc(p2), st_sfc(p3)), crs = 4326)

edges$from = c(1, 1, 3)
edges$to = c(2, 3, 2)

net = sfnetwork(nodes, edges)
net
class(net)

## -----------------------------------------------------------------------------
net = sfnetwork(nodes, edges, directed = FALSE)
net

## -----------------------------------------------------------------------------
nodes$name = c("city", "village", "farm")
edges$from = c("city", "city", "farm")
edges$to = c("village", "farm", "village")

edges

net = sfnetwork(nodes, edges, node_key = "name")
net

## ---- fig.show='hold', out.width='50%'----------------------------------------
st_geometry(edges) = NULL

other_net = sfnetwork(nodes, edges, edges_as_lines = TRUE)

plot(net, cex = 2, lwd = 2, main = "Original geometries")
plot(other_net, cex = 2, lwd = 2, main = "Straight lines")

## ---- error=TRUE--------------------------------------------------------------
st_geometry(edges) = st_sfc(c(l2, l3, l1), crs = 4326)

net = sfnetwork(nodes, edges)

## ---- fig.height=5, fig.width=5-----------------------------------------------
roxel
net = as_sfnetwork(roxel)
plot(net)

## -----------------------------------------------------------------------------
net %>%
  activate("edges") %>%
  mutate(weight = edge_length()) %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness(weights = weight, directed = FALSE))

## -----------------------------------------------------------------------------
net %>%
  activate("nodes") %>%
  st_as_sf()

## -----------------------------------------------------------------------------
st_as_sf(net, "edges")

## ---- fig.width=5, fig.height=5-----------------------------------------------
plot(net)

## ---- message=FALSE, fig.width=5, fig.height=5--------------------------------
autoplot(net) + ggtitle("Road network of Münster Roxel")

## ---- fig.height=5, fig.width=5-----------------------------------------------
net = net %>%
  activate("nodes") %>%
  mutate(bc = centrality_betweenness())

ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), col = "grey50") +
  geom_sf(data = st_as_sf(net, "nodes"), aes(col = bc, size = bc)) +
  ggtitle("Betweenness centrality in Münster Roxel")

## -----------------------------------------------------------------------------
net %>%
  activate("nodes") %>%
  st_geometry()

## ---- fig.show = 'hold', out.width = "50%"------------------------------------
net %>%
  activate("edges") %>%
  st_set_geometry(NULL) %>%
  plot(draw_lines = FALSE, main = "Edges without geometries")

net %>%
  activate("nodes") %>%
  st_set_geometry(NULL) %>%
  plot(vertex.color = "black", main = "Nodes without geometries")

## ---- eval = geos37-----------------------------------------------------------
as_sfnetwork(roxel, directed = TRUE) %>%
  activate("edges") %>%
  st_reverse()

## -----------------------------------------------------------------------------
node_coords = net %>%
  activate("nodes") %>%
  st_coordinates()

node_coords[1:4, ]

## -----------------------------------------------------------------------------
# Currently there are neither Z nor M coordinates.
st_z_range(net)
st_m_range(net)

# Add Z coordinates with value 0 to all features.
# This will affect both nodes and edges, no matter which element is active.
st_zm(net, drop = FALSE, what = "Z")

## -----------------------------------------------------------------------------
net %>%
  st_zm(drop = FALSE, what = "Z") %>%
  mutate(X = node_X(), Y = node_Y(), Z = node_Z(), M = node_M())

## -----------------------------------------------------------------------------
st_crs(net)

## -----------------------------------------------------------------------------
st_transform(net, 3035)

## -----------------------------------------------------------------------------
st_precision(net)

## -----------------------------------------------------------------------------
net %>%
  st_set_precision(1) %>%
  st_precision()

## -----------------------------------------------------------------------------
net %>%
  activate("nodes") %>%
  st_bbox()

## ---- fig.show='hold', out.width = "50%"--------------------------------------
node1 = st_point(c(8, 51))
node2 = st_point(c(7, 51.5))
node3 = st_point(c(8, 52))
node4 = st_point(c(9, 51))
edge1 = st_sfc(st_linestring(c(node1, node2, node3)))

nodes = st_as_sf(c(st_sfc(node1), st_sfc(node3), st_sfc(node4)))
edges = st_as_sf(edge1)
edges$from = 1
edges$to = 2

small_net = sfnetwork(nodes, edges)

node_bbox = st_as_sfc(st_bbox(activate(small_net, "nodes")))
edge_bbox = st_as_sfc(st_bbox(activate(small_net, "edges")))
net_bbox = st_as_sfc(st_network_bbox(small_net))

plot(small_net, lwd = 2, cex = 4, main = "Element bounding boxes")
plot(node_bbox, border = "red", lty = 2, lwd = 4, add = TRUE)
plot(edge_bbox, border = "blue", lty = 2, lwd = 4, add = TRUE)
plot(small_net, lwd = 2, cex = 4, main = "Network bounding box")
plot(net_bbox, border = "red", lty = 2, lwd = 4, add = TRUE)

## -----------------------------------------------------------------------------
net %>%
  activate("edges") %>%
  st_set_agr(c("name" = "constant", "type" = "constant")) %>%
  st_agr()

## ---- include = FALSE---------------------------------------------------------
par(oldpar)
options(oldoptions)

