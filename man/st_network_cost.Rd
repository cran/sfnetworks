% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paths.R
\name{st_network_cost}
\alias{st_network_cost}
\title{Compute a cost matrix of a spatial network}
\usage{
st_network_cost(
  x,
  from = igraph::V(x),
  to = igraph::V(x),
  weights = NULL,
  direction = "out",
  Inf_as_NaN = FALSE,
  ...
)
}
\arguments{
\item{x}{An object of class \code{\link{sfnetwork}}.}

\item{from}{The (set of) geospatial point(s) from which the shortest paths
will be calculated. Can be an object of  class \code{\link[sf]{sf}} or
\code{\link[sf]{sfc}}.
Alternatively it can be a numeric vector containing the indices of the nodes
from which the shortest paths will be calculated, or a character vector
containing the names of the nodes from which the shortest paths will be
calculated. By default, all nodes in the network are included.}

\item{to}{The (set of) geospatial point(s) to which the shortest paths will
be calculated. Can be an object of class \code{\link[sf]{sf}} or
\code{\link[sf]{sfc}}.
Alternatively it can be a numeric vector containing the indices of the nodes
to which the shortest paths will be calculated, or a character vector
containing the names of the nodes to which the shortest paths will be
calculated. Duplicated values will be removed before calculating the cost
matrix. By default, all nodes in the network are included.}

\item{weights}{The edge weights to be used in the shortest path calculation.
Can be a numeric vector giving edge weights, or a column name referring to
an attribute column in the edges table containing those weights. If set to
\code{NULL}, the values of a column named \code{weight} in the edges table
will be used automatically, as long as this column is present. If not, the
geographic edge lengths will be calculated internally and used as weights.
If set to \code{NA}, no weights are used, even if the edges have a
\code{weight} column.}

\item{direction}{The direction of travel. Defaults to \code{'out'}, meaning
that the direction given by the network is followed and costs are calculated
from the points given as argument \code{from}. May be set to \code{'in'},
meaning that the opposite direction is followed an costs are calculated
towards the points given as argument \code{from}. May also be set to
\code{'all'}, meaning that the network is considered to be undirected. This
argument is ignored for undirected networks.}

\item{Inf_as_NaN}{Should the cost values of unconnected nodes be stored as
\code{NaN} instead of \code{Inf}? Defaults to \code{FALSE}.}

\item{...}{Arguments passed on to \code{\link[igraph]{distances}}. Argument
\code{mode} is ignored. Use \code{direction} instead.}
}
\value{
An n times m numeric matrix where n is the length of the \code{from}
argument, and m is the length of the \code{to} argument.
}
\description{
Wrapper around \code{\link[igraph]{distances}} to calculate costs of
pairwise shortest paths between points in a spatial network. It allows to
provide any set of geospatial point as \code{from} and \code{to} arguments.
If such a geospatial point is not equal to a node in the network, it will
be snapped to its nearest node before calculating costs.
}
\details{
Spatial features provided to the \code{from} and/or
\code{to} argument don't necessarily have to be points. Internally, the
nearest node to each feature is found by calling
\code{\link[sf]{st_nearest_feature}}, so any feature with a geometry type
that is accepted by that function can be provided as \code{from} and/or
\code{to} argument.

When directly providing integer node indices or character node names to the
\code{from} and/or \code{to} argument, keep the following in mind. A node
index should correspond to a row-number of the nodes table of the network.
A node name should correspond to a value of a column in the nodes table
named \code{name}. This column should contain character values without
duplicates.

For more details on the wrapped function from \code{\link[igraph]{igraph}}
see the \code{\link[igraph]{distances}} documentation page.
}
\examples{
library(sf, quietly = TRUE)
library(tidygraph, quietly = TRUE)

# Create a network with edge lengths as weights.
# These weights will be used automatically in shortest paths calculation.
net = as_sfnetwork(roxel, directed = FALSE) \%>\%
  st_transform(3035) \%>\%
  activate("edges") \%>\%
  mutate(weight = edge_length())

# Providing node indices.
st_network_cost(net, from = c(495, 121), to = c(495, 121))

# Providing nodes as spatial points.
# Points that don't equal a node will be snapped to their nearest node.
p1 = st_geometry(net, "nodes")[495] + st_sfc(st_point(c(50, -50)))
st_crs(p1) = st_crs(net)
p2 = st_geometry(net, "nodes")[121] + st_sfc(st_point(c(-10, 100)))
st_crs(p2) = st_crs(net)

st_network_cost(net, from = c(p1, p2), to = c(p1, p2))

# Using another column for weights.
net \%>\%
  activate("edges") \%>\%
  mutate(foo = runif(n(), min = 0, max = 1)) \%>\%
  st_network_cost(c(p1, p2), c(p1, p2), weights = "foo")

# Not providing any from or to points includes all nodes by default.
with_graph(net, graph_order()) # Our network has 701 nodes.
cost_matrix = st_network_cost(net)
dim(cost_matrix)

}
\seealso{
\code{\link{st_network_paths}}
}
