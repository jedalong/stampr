#' Hurricane Katrina polygons dataset
#'
#' A dataset containing polygons representing the movement of Hurricane Katrina from 21:00 26-AUG-2005 to 
#' 21:00 29-AUG-2005. Polygon contours were extracted from the US NOAA H*Wind product, downloadable from:
#' \url{https://www.aoml.noaa.gov/hrd/data_sub/wind.html}
#' 
#' The \code{katrina} dataset contains polygons that were derived from the raw NOAA H*Wind data. The 39 mph isotach
#' (contour of equal wind speed) was used to delineate, as a spatial polygon, the extent of Hurricane Katrina 
#' at a given time. Polygons were derived at 3 hr intervals; which means there are 33 different time points in
#' the dataset. 
#'
#' @references
#'  Powell, M.D., Murillo, S., Dodge, P., Uhlhorn, E., Gamache, J., Cardone, V., Cox, A., Otero, S., Carrasco, N., 
#'  Annane, B., St. Fleur, R. (2010) Reconstruction of Hurricane Katrina's wind fields for storm surge and wave 
#'  hindcasting. \emph{Ocean Engineering}, 37, 26-36. \cr\cr
#'  Powell, M.D., Houston, S.H. (1998) The HMD real-time hurricane wind analysis system. \emph{Journal of Wind
#'  Engineering and Industrial Aerodynamics}, 77/78, 53-64.
#'  
#' @source \url{https://www.aoml.noaa.gov/hrd/data_sub/wind.html}
#'
#' @docType data
#' @keywords datasets
#' @format A \code{sf} object with 33 records of of the location of Hurricane Katrina, every 
#'  3 hrs, from 21:00 25-AUG-2005 to 21:00 29-AUG-2005. The date and time of each polygon is recorded in the
#'  column \code{DateTime}.
#' @name katrina
#' @examples
#' data(katrina)
#' plot(katrina['Id'])
NULL
