# This is package documentation for stampr.
# roxygen will use this file to create a NAMESPACE file.
# Of importance is the @import command, as it lists package dependencies.

#' stampr: Spatial Temporal Analysis of Moving Polygons
#'
#' The Package \code{stampr} provides tools for performing spatial temporal analysis of moving polygons.
#' These tools allow the calculation of directional relationships, shape indices, and other basic
#' functionality, such as global change metrics. More details about each of these functions
#' can be found in its help documentation.
#'
#' \code{stampr}'s functions utilize the \code{SpatialPolygonsDataFrame} objects from the 
#' package \code{sp}. Polygon relationships are still understudied in the field of 
#' geographic information science, but hopefully \code{stampr} can provide users with a
#' platform for new developments and applied research looking at interesting geographical phenomena.
#'
#' @author Jed Long and Colin Robertson
#' @references
#' Robertson, C., Nelson, T., Boots, B., and Wulder, M. (2007) STAMP: Spatial-temporal analysis of moving polygons.
#'  \emph{Journal of Geographical Systems}, 9:207-227.
#'
#' @import sf lwgeom spdep  grDevices methods
# @importFrom stats optim
#' @docType package
#' @name stampr-package
NULL
