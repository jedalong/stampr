% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stamp.multichange.r
\name{stamp.multichange}
\alias{stamp.multichange}
\title{run stamp function for multiple years of polygons at once}
\usage{
stamp.multichange(polys, changeByRow = TRUE, changeField = "", ...)
}
\arguments{
\item{polys}{a \code{sf} polygon object with 2+ years of data to run through the \code{stamp} function.}

\item{changeByRow}{logical, whether or not each time period is a separate unique row of data (e.g., as per the \code{katrina} data)}

\item{changeField}{string, name of the field which contains time period if changeByRow is FALSE}

\item{...}{list of paramater values to provide to the \code{stamp} function}
}
\value{
A \code{sf} object which includes all outputs from the calls to the \code{stamp} function. If there are T time periods,
 there will be T-1 time periods in the resulting \code{sf} object.
}
\description{
The function \code{stamp.multichange} is a wrapper function that makes multiple calls to the stamp 
 function to ease spatial-temporal analysis of multiple years of polygon data
}
\details{
\code{stamp.multichange} is a simple wrapper function for the \code{stamp} function. The two options for data structure
 are those in the \code{katrina} data, where each time period is a row, and rows are time-ordered, and the structure of 
 the \code{mpb} data, where time period is specified by a column. Time periods should be ordered from 1 through T.
}
\examples{
\dontrun{
## NOT RUN ##
data("katrina")
ch <- stamp.multichange(katrina, changeByRow = TRUE, dc = 0, distance = TRUE, direction = FALSE)
STGroup <- stamp.stgroup.summary(ch)
head(STGroup)
}
}
\seealso{
stamp.stgroup.summary
}
\keyword{stamp}
