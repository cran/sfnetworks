% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf.R
\name{sf}
\alias{sf}
\alias{st_as_sf.sfnetwork}
\alias{st_as_s2.sfnetwork}
\alias{st_geometry.sfnetwork}
\alias{st_geometry<-.sfnetwork}
\alias{st_drop_geometry.sfnetwork}
\alias{st_bbox.sfnetwork}
\alias{st_coordinates.sfnetwork}
\alias{st_is.sfnetwork}
\alias{st_is_valid.sfnetwork}
\alias{st_crs.sfnetwork}
\alias{st_crs<-.sfnetwork}
\alias{st_precision.sfnetwork}
\alias{st_set_precision.sfnetwork}
\alias{st_shift_longitude.sfnetwork}
\alias{st_transform.sfnetwork}
\alias{st_wrap_dateline.sfnetwork}
\alias{st_normalize.sfnetwork}
\alias{st_zm.sfnetwork}
\alias{st_m_range.sfnetwork}
\alias{st_z_range.sfnetwork}
\alias{st_agr.sfnetwork}
\alias{st_agr<-.sfnetwork}
\alias{st_reverse.sfnetwork}
\alias{st_simplify.sfnetwork}
\alias{st_join.sfnetwork}
\alias{st_join.morphed_sfnetwork}
\alias{st_filter.sfnetwork}
\alias{st_filter.morphed_sfnetwork}
\alias{st_crop.sfnetwork}
\alias{st_crop.morphed_sfnetwork}
\alias{st_difference.sfnetwork}
\alias{st_difference.morphed_sfnetwork}
\alias{st_intersection.sfnetwork}
\alias{st_intersection.morphed_sfnetwork}
\alias{st_intersects.sfnetwork}
\alias{st_sample.sfnetwork}
\alias{st_nearest_points.sfnetwork}
\alias{st_area.sfnetwork}
\title{sf methods for sfnetworks}
\usage{
\method{st_as_sf}{sfnetwork}(x, active = NULL, ...)

\method{st_as_s2}{sfnetwork}(x, active = NULL, ...)

\method{st_geometry}{sfnetwork}(obj, active = NULL, ...)

\method{st_geometry}{sfnetwork}(x) <- value

\method{st_drop_geometry}{sfnetwork}(x, ...)

\method{st_bbox}{sfnetwork}(obj, active = NULL, ...)

\method{st_coordinates}{sfnetwork}(x, active = NULL, ...)

\method{st_is}{sfnetwork}(x, ...)

\method{st_is_valid}{sfnetwork}(x, ...)

\method{st_crs}{sfnetwork}(x, ...)

\method{st_crs}{sfnetwork}(x) <- value

\method{st_precision}{sfnetwork}(x)

\method{st_set_precision}{sfnetwork}(x, precision)

\method{st_shift_longitude}{sfnetwork}(x, ...)

\method{st_transform}{sfnetwork}(x, ...)

\method{st_wrap_dateline}{sfnetwork}(x, ...)

\method{st_normalize}{sfnetwork}(x, ...)

\method{st_zm}{sfnetwork}(x, ...)

\method{st_m_range}{sfnetwork}(obj, active = NULL, ...)

\method{st_z_range}{sfnetwork}(obj, active = NULL, ...)

\method{st_agr}{sfnetwork}(x, active = NULL, ...)

\method{st_agr}{sfnetwork}(x) <- value

\method{st_reverse}{sfnetwork}(x, ...)

\method{st_simplify}{sfnetwork}(x, ...)

\method{st_join}{sfnetwork}(x, y, ...)

\method{st_join}{morphed_sfnetwork}(x, y, ...)

\method{st_filter}{sfnetwork}(x, y, ...)

\method{st_filter}{morphed_sfnetwork}(x, y, ...)

\method{st_crop}{sfnetwork}(x, y, ...)

\method{st_crop}{morphed_sfnetwork}(x, y, ...)

\method{st_difference}{sfnetwork}(x, y, ...)

\method{st_difference}{morphed_sfnetwork}(x, y, ...)

\method{st_intersection}{sfnetwork}(x, y, ...)

\method{st_intersection}{morphed_sfnetwork}(x, y, ...)

\method{st_intersects}{sfnetwork}(x, y, ...)

\method{st_sample}{sfnetwork}(x, ...)

\method{st_nearest_points}{sfnetwork}(x, y, ...)

\method{st_area}{sfnetwork}(x, ...)
}
\arguments{
\item{x}{An object of class \code{\link{sfnetwork}}.}

\item{active}{Which network element (i.e. nodes or edges) to activate before
extracting. If \code{NULL}, it will be set to the current active element of
the given network. Defaults to \code{NULL}.}

\item{...}{Arguments passed on the corresponding \code{sf} function.}

\item{obj}{An object of class \code{\link{sfnetwork}}.}

\item{value}{The value to be assigned. See the documentation of the
corresponding sf function for details.}

\item{precision}{The precision to be assigned. See
\code{\link[sf]{st_precision}} for details.}

\item{y}{An object of class \code{\link[sf]{sf}}, or directly convertible to
it using \code{\link[sf]{st_as_sf}}. In some cases, it can also be an object
of \code{\link[sf:st]{sfg}} or \code{\link[sf:st_bbox]{bbox}}. Always look
at the documentation of the corresponding \code{sf} function for details.}
}
\value{
The \code{sfnetwork} method for \code{\link[sf]{st_as_sf}} returns
the active element of the network as object of class \code{\link[sf]{sf}}.
The \code{sfnetwork} and \code{morphed_sfnetwork} methods for
\code{\link[sf]{st_join}}, \code{\link[sf]{st_filter}},
\code{\link[sf]{st_intersection}}, \code{\link[sf]{st_difference}},
\code{\link[sf]{st_crop}} and the setter functions
 return an object of class \code{\link{sfnetwork}}
and \code{morphed_sfnetwork} respectively. All other
methods return the same type of objects as their corresponding sf function.
See the \code{\link[sf]{sf}} documentation for details.
}
\description{
\code{\link[sf]{sf}} methods for \code{\link{sfnetwork}} objects.
}
\details{
See the \code{\link[sf]{sf}} documentation.
}
\examples{
library(sf, quietly = TRUE)

net = as_sfnetwork(roxel)

# Extract the active network element.
st_as_sf(net)

# Extract any network element.
st_as_sf(net, "edges")

# Get geometry of the active network element.
st_geometry(net)

# Get geometry of any network element.
st_geometry(net, "edges")

# Get bbox of the active network element.
st_bbox(net)

# Get CRS of the network.
st_crs(net)

# Get agr factor of the active network element.
st_agr(net)

# Get agr factor of any network element.
st_agr(net, "edges")

# Spatial join applied to the active network element.
net = st_transform(net, 3035)
codes = st_as_sf(st_make_grid(net, n = c(2, 2)))
codes$post_code = as.character(seq(1000, 1000 + nrow(codes) * 10 - 10, 10))

joined = st_join(net, codes, join = st_intersects)
joined

oldpar = par(no.readonly = TRUE)
par(mar = c(1,1,1,1), mfrow = c(1,2))
plot(net, col = "grey")
plot(codes, col = NA, border = "red", lty = 4, lwd = 4, add = TRUE)
text(st_coordinates(st_centroid(st_geometry(codes))), codes$post_code)
plot(st_geometry(joined, "edges"))
plot(st_as_sf(joined, "nodes"), pch = 20, add = TRUE)
par(oldpar)
# Spatial filter applied to the active network element.
p1 = st_point(c(4151358, 3208045))
p2 = st_point(c(4151340, 3207520))
p3 = st_point(c(4151756, 3207506))
p4 = st_point(c(4151774, 3208031))

poly = st_multipoint(c(p1, p2, p3, p4)) \%>\%
  st_cast('POLYGON') \%>\%
  st_sfc(crs = 3035) \%>\%
  st_as_sf()

filtered = st_filter(net, poly, .pred = st_intersects)

oldpar = par(no.readonly = TRUE)
par(mar = c(1,1,1,1), mfrow = c(1,2))
plot(net, col = "grey")
plot(poly, border = "red", lty = 4, lwd = 4, add = TRUE)
plot(filtered)
par(oldpar)
}
