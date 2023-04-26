#' Hurricane Katrina eye point dataset
#'
#' A dataset containing points representing the eye of Hurricane Katrina centroid from 21:00 26-AUG-2005 to 
#' 21:00 29-AUG-2005. Polygon contours were extracted from the US NOAA H*Wind product, downloadable from:
#' \url{https://www.aoml.noaa.gov/hrd/data_sub/wind.html}
#' 
#' The \code{eyeshp} dataset contains points that were derived from the raw NOAA H*Wind data. The data is included here
#' to provide a point-data comparison to the data in the \code{katrina} dataset which is polygon data 
#'
#'
#' @docType data
#' @keywords datasets
#' @format A \code{sf} object with 33 records of the eye location of Hurricane Katrina, every 
#'  3 hrs, from 21:00 25-AUG-2005 to 21:00 29-AUG-2005. The date and time of each polygon is recorded in the
#'  column \code{DateTime}.
#' @name eyeshp
#' @examples
#' data(eyeshp)
#' plot(eyeshp)
NULL
