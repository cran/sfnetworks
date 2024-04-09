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

## ----message=FALSE------------------------------------------------------------
library(sfnetworks)
library(sf)
library(tidygraph)
library(dplyr)
library(purrr)
library(TSP)

## ----fig.height=5, fig.width=5------------------------------------------------
net = as_sfnetwork(roxel, directed = FALSE) %>%
  st_transform(3035) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

paths = st_network_paths(net, from = 495, to = c(458, 121), weights = "weight")
paths
paths %>%
  slice(1) %>%
  pull(node_paths) %>%
  unlist()

paths %>%
  slice(1) %>%
  pull(edge_paths) %>%
  unlist()

plot_path = function(node_path) {
  net %>%
    activate("nodes") %>%
    slice(node_path) %>%
    plot(cex = 1.5, lwd = 1.5, add = TRUE)
}

colors = sf.colors(3, categorical = TRUE)

plot(net, col = "grey")
paths %>%
  pull(node_paths) %>%
  walk(plot_path)
net %>%
  activate("nodes") %>%
  st_as_sf() %>%
  slice(c(495, 121, 458)) %>%
  plot(col = colors, pch = 8, cex = 2, lwd = 2, add = TRUE)

## ----fig.height=5, fig.width=5------------------------------------------------
p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
st_crs(p1) = st_crs(net)
p2 = st_geometry(net, "nodes")[458]
p3 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
st_crs(p3) = st_crs(net)

paths = st_network_paths(net, from = p1, to = c(p2, p3), weights = "weight")

plot(net, col = "grey")
paths %>%
  pull(node_paths) %>%
  walk(plot_path)
plot(c(p1, p2, p3), col = colors, pch = 8, cex = 2, lwd = 2, add = TRUE)

## ----fig.height=5, fig.width=5------------------------------------------------
# Our network consists of several unconnected components.
with_graph(net, graph_component_count())

connected_net = net %>%
  activate("nodes") %>%
  filter(group_components() == 1)

plot(net, col = colors[2])
plot(connected_net, cex = 1.1, lwd = 1.1, add = TRUE)

## -----------------------------------------------------------------------------
st_network_cost(net, from = c(p1, p2, p3), to = c(p1, p2, p3), weights = "weight")

## -----------------------------------------------------------------------------
# Our network has 701 nodes.
with_graph(net, graph_order())

cost_matrix = st_network_cost(net, weights = "weight")
dim(cost_matrix)

## -----------------------------------------------------------------------------
net %>% 
  convert(to_spatial_directed) %>% 
  st_network_cost(
    from = c(p1, p2, p3),
    to = c(p1, p2, p3),
    direction = "in"
  )

net %>% 
  convert(to_spatial_directed) %>% 
  st_network_cost(
    from = c(p1, p2, p3),
    to = c(p1, p2, p3),
    direction = "out"
  )

net %>% 
  convert(to_spatial_directed) %>% 
  st_network_cost(
    from = c(p1, p2, p3),
    to = c(p1, p2, p3),
    direction = "all"
  )

## ----fig.height=5, fig.width=5------------------------------------------------
# Select a random set of sites and facilities.
# We select random locations within the bounding box of the nodes.
set.seed(128)
hull = net %>%
  activate("nodes") %>%
  st_geometry() %>%
  st_combine() %>%
  st_convex_hull()

sites = st_sample(hull, 50, type = "random")
facilities = st_sample(hull, 5, type = "random")

# Blend the sites and facilities into the network to get better results.
# Also select only the largest connected component.
new_net = net %>%
  activate("nodes") %>%
  filter(group_components() == 1) %>%
  st_network_blend(c(sites, facilities))

# Calculate the cost matrix.
cost_matrix = st_network_cost(new_net, from = sites, to = facilities, weights = "weight")

# Find for each site which facility is closest.
closest = facilities[apply(cost_matrix, 1, function(x) which(x == min(x))[1])]

# Create a line between each site and its closest facility, for visualization.
draw_lines = function(sources, targets) {
  lines = mapply(
    function(a, b) st_sfc(st_cast(c(a, b), "LINESTRING"), crs = st_crs(net)),
    sources,
    targets,
    SIMPLIFY = FALSE
  )
  do.call("c", lines)
}

connections = draw_lines(sites, closest)

# Plot the results.
plot(new_net, col = "grey")
plot(connections, col = colors[2], lwd = 2, add = TRUE)
plot(facilities, pch = 8, cex = 2, lwd = 2, col = colors[3], add = TRUE)
plot(sites, pch = 20, cex = 2, col = colors[2], add = TRUE)

## -----------------------------------------------------------------------------
set.seed(403)
rdm = net %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_sample(10, type = "random")

## -----------------------------------------------------------------------------
net = activate(net, "nodes")
cost_matrix = st_network_cost(net, from = rdm, to = rdm, weights = "weight")

# Use nearest node indices as row and column names.
rdm_idxs = st_nearest_feature(rdm, net)
row.names(cost_matrix) = rdm_idxs
colnames(cost_matrix) = rdm_idxs

round(cost_matrix, 0)

## -----------------------------------------------------------------------------
tour = solve_TSP(TSP(units::drop_units(cost_matrix)))
tour_idxs = as.numeric(names(tour))
tour_idxs

# Approximate length of the route.
# In meters, since that was the unit of our cost values.
round(tour_length(tour), 0)

## ----fig.height=5, fig.width=5------------------------------------------------
# Define the nodes to calculate the shortest paths from.
# Define the nodes to calculate the shortest paths to.
# All based on the calculated order of visit.
from_idxs = tour_idxs
to_idxs = c(tour_idxs[2:length(tour_idxs)], tour_idxs[1])

# Calculate the specified paths.
tsp_paths = mapply(st_network_paths,
    from = from_idxs,
    to = to_idxs,
    MoreArgs = list(x = net, weights = "weight")
  )["node_paths", ] %>%
  unlist(recursive = FALSE)

# Plot the results.
plot(net, col = "grey")
plot(rdm, pch = 20, col = colors[2], add = TRUE)

walk(tsp_paths, plot_path) # Reuse the plot_path function defined earlier.

plot(
  st_as_sf(slice(net, rdm_idxs)),
  pch = 20, col = colors[3], add = TRUE
)
plot(
  st_as_sf(slice(net, tour_idxs[1])),
  pch = 8, cex = 2, lwd = 2, col = colors[3], add = TRUE
)
text(
  st_coordinates(st_as_sf(slice(net, tour_idxs[1]))) - c(200, 90),
  labels = "start/end\npoint"
)

## ----warning=FALSE------------------------------------------------------------
# How many edge types are there?
types = net %>%
  activate("edges") %>%
  pull(type) %>%
  unique()

types

# Randomly define a walking speed in m/s for each type.
# With values between 3 and 7 km/hr.
set.seed(1)
speeds = runif(length(types), 3 * 1000 / 60 / 60, 7 * 1000 / 60 / 60)

# Assign a speed to each edge based on its type.
# Calculate travel time for each edge based on that.
net = net %>%
  activate("edges") %>%
  group_by(type) %>%
  mutate(speed = units::set_units(speeds[cur_group_id()], "m/s")) %>%
  mutate(time = weight / speed) %>%
  ungroup()

net

## ----fig.height=5, fig.width=5------------------------------------------------
net = activate(net, "nodes")

p = net %>%
  st_geometry() %>%
  st_combine() %>%
  st_centroid()

iso = net %>%
  filter(node_distance_from(st_nearest_feature(p, net), weights = time) <= 600)

iso_poly = iso %>%
  st_geometry() %>%
  st_combine() %>%
  st_convex_hull()

plot(net, col = "grey")
plot(iso_poly, col = NA, border = "black", lwd = 3, add = TRUE)
plot(iso, col = colors[2], add = TRUE)
plot(p, col = colors[1], pch = 8, cex = 2, lwd = 2, add = TRUE)

## ----fig.width=5, fig.height=5------------------------------------------------
# Define the threshold values (in seconds).
# Define also the colors to plot the neighborhoods in.
thresholds = rev(seq(60, 600, 60))
palette = sf.colors(n = 10)

# Plot the results.
plot(net, col = "grey")

for (i in c(1:10)) {
  nbh = convert(net, to_spatial_neighborhood, p, thresholds[i], weights = "time")
  plot(nbh, col = palette[i], add = TRUE)
}

plot(p, pch = 8, cex = 2, lwd = 2, add = TRUE)

## -----------------------------------------------------------------------------
table(roxel$type)

## ----fig.height=5, fig.width=5------------------------------------------------
paths_sf = net %>%
  activate("edges") %>%
  slice(unlist(paths$edge_paths)) %>%
  st_as_sf()

table(paths_sf$type)

plot(paths_sf["type"], lwd = 4, key.pos = 4, key.width = lcm(4))

## -----------------------------------------------------------------------------
weighting_profile = c(
  cycleway = Inf,
  footway = Inf,
  path = Inf,
  pedestrian = Inf,
  residential = 3,
  secondary = 1,
  service = 1,
  track = 10,
  unclassified = 1
)

weighted_net = net %>%
  activate("edges") %>%
  mutate(multiplier = weighting_profile[type]) %>%
  mutate(weight = edge_length() * multiplier)

## ----fig.show='hold', out.width = '50%'---------------------------------------
weighted_paths = st_network_paths(weighted_net, from = 495, to = c(458, 121), weights = "weight")

weighted_paths_sf = weighted_net %>%
  activate("edges") %>%
  slice(unlist(weighted_paths$edge_paths)) %>%
  st_as_sf()

table(weighted_paths_sf$type)

plot(st_as_sf(net, "edges")["type"], lwd = 4,
     key.pos = NULL, reset = FALSE, main = "Distance weights")
plot(st_geometry(paths_sf), add = TRUE)
plot(st_as_sf(net, "edges")["type"], lwd = 4,
     key.pos = NULL, reset = FALSE, main = "Custom weights")
plot(st_geometry(weighted_paths_sf), add = TRUE)

## ----include = FALSE----------------------------------------------------------
par(oldpar)
options(oldoptions)

