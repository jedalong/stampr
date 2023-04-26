#' MPB dataset
#'
#' A dataset containing polygons representing the location of mountain pine beetle hotspot polygons in Morice Forest District, British Columbia, Canada. 
#' 
#' These data were derived from helicopter-based GPS surveys during early years of large mountain pine beetle outbreak in Western Canada. 
#'
#' @references
#' Nelson TA, Boots B, Wulder MA, Carroll AL. Environmental characteristics of mountain pine beetle infestation hot spots. \emph{Journal of Ecosystems and Management}. 2007 Mar 14;8(1).
#'  
#' @source Data obtained from Trisalyn Nelson (ASU)
#'
#' @docType data
#' @keywords datasets
#' @format \code{mpb} --- a \code{sf} object with 711 mountain pine beetle hotspot polygons that occurred over eight years. The temporal indicator is the \code{TGROUP} column. Another variable \code{REGION} indicates whether the hotspot was in the northern or southern regions, which experienced mostly independent outbreaks. 
#' @name mpb
#' @examples
#' data(mpb)
#' plot(mpb['TGROUP'])
NULL
