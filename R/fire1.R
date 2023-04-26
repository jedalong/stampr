#' Forest Fire dataset
#'
#' A dataset containing fake forest fire polygons representing the movement of the forest fire from T1 (fire1) to T2 (fire2). The data is provided purely for demonstration purposes.
#' 
#'  
#'
#'  
#' @source Simulated data
#'
#' @docType data
#' @keywords datasets
#' @format \code{fire1} --- a \code{sf} object with polygons representing the location of forest fire. 
#' @name fire1
#' @examples
#' 
#' data(fire1)
#' plot(fire1)
#' 
#' data(fire2)
#' plot(fire2)
#' 
#' \dontrun{
#' library(mapview)
#' mapview(fire1) + mapview(fire2)
#' }
NULL
