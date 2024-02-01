# This is package documentation for stampr.
# roxygen will use this file to create a NAMESPACE file.
# Of importance is the @import command, as it lists package dependencies.

#' stampr: Spatial Temporal Analysis of Moving Polygons
#'
#' The Package \code{stampr} provides tools for performing spatial temporal analysis of moving polygons.
#' These tools allow the calculation of directional relationships, distance relations, and other basic
#' functionality, such as global change metrics. More details about each of these functions
#' can be found in its help documentation.
#'
#' \code{stampr}'s functions utilize the \code{sf} objects (as of version 0.3.0) from the 
#' package \code{sf}. Polygon relationships are still understudied in the field of 
#' geographic information science, but hopefully \code{stampr} can provide users with a
#' platform for new developments and applied research looking at interesting geographical phenomena.
#'
#' @author Jed Long and Colin Robertson
#' @references
#' Robertson, C., Nelson, T., Boots, B., and Wulder, M. (2007) STAMP: Spatial-temporal analysis of moving polygons
#'  \emph{Journal of Geographical Systems}, 9:207-227.
#' Long, J., Robertson, C., Nelson, T. (2018) stampr: Spatial-Temporal Analysis of Moving Polygons in R \emph{Journal of Statistical Software}. Code Snippets, 84(1), 1–19.
#' 
#' @import sf
#' @import spdep
#' @import dplyr
#' @importFrom lwgeom st_geod_azimuth
#' @importFrom geosphere geodesic
#' @importFrom grDevices palette rainbow
#' @importFrom graphics lcm text
#' @importFrom rlang .data
#' @docType package
#' @name stampr-package
NULL
