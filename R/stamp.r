# ---- roxygen documentation ----
#
#' @title Spatial temporal analysis of moving polygons
#'
#' @description
#' This function generates a \code{sf} polygons object that can be used for spatial temporal analysis of moving polygons
#' as described in the paper Robertson et al. (2007).
#'
#' @details
#'  The \code{stamp} function can be used to perform spatial temporal analysis of moving polygons (STAMP)
#'  as outlined in the paper by Robertson et al., (2007). Polygon movement "groups" are delineated based on
#'  polygon connectedness defined by the distance threshold \code{dc}. That is, if polygon
#'  boundaries (in T1 or T2) are within distance \code{dc} of one another they will be designated
#'  to the same group. STAMP events are reported at four levels of increasing complexity: \cr
#'  LEV1 -- disappearance (\code{DISA}), stable (\code{STBL}), and generation (\code{GENA}); \cr
#'  LEV2 -- disappearance (\code{DISA}), contraction (\code{CONT}), stable (\code{STBL}),
#'          expansion (\code{EXPN}), and generation (\code{GENR}); \cr
#'  LEV3 -- disappearance (\code{DISA}), T1 displacement (\code{DISP1}), convergence (\code{CONV}),
#'          concentration (\code{CONC}), contraction (\code{CONT}), stable (\code{STBL}),
#'          expansion (\code{EXP}), fragmentation (\code{FRAG}), divergence (\code{DIV}),
#'          T2 displacement (\code{DISP2}), and generation (\code{GENR}); \cr
#'  LEV4 -- LEV4 is different from other levels. It is used to identify those groups where
#'          union (\code{UNION}), division (\code{DIVISION}), and both union and division
#'          (\code{BOTH}) events occur. These events occur when there are more than one
#'          stable event in a group. Groups with one or no stable events receive an \code{NA}
#'          value for LEV4. \cr
#'  See Robertson et al. (2007; especially Figure 1) for complete descriptions of all STAMP movement
#'  event types.
#'  
#'  Note also that there must be a unique ID of each polygon, the function uses the row.names of the polygon objects. Modify the row.names accordingly if you wish to use an alternative ID label.
#'
#' @param T1 a \code{sf} polygons object of polygons from time 1.
#' @param T2 a \code{sf} polygons object of polygons from time 2.
#' @param dc spatial distance threshold for determining groupings (see \bold{Details}) in appropriate units.
#' @param direction logical, whether or not to perform directional analysis. See documentation for
#'    \code{stamp.direction} for further details.
#' @param distance logical, whether or not to perform distance analysis. See documentation for 
#'    \code{stamp.distance} for further details.
#' @param ... additional parameters to be passed to functions if \code{direction}, or \code{distance} are
#'    set to \code{TRUE}. 
#'
#'
#' @return
#'  This function returns a \code{sf} polygons object with the following data columns:
#'  \item{ID1}{Polygon ID from T1 polygons; \code{NA} if it did not exist,}
#'  \item{ID2}{Polygon ID from T2 polygons; \code{NA} if it did not exist,}
#'  \item{LEV1}{Level 1 STAMP designation,}
#'  \item{LEV2}{Level 2 STAMP designation,}
#'  \item{LEV3}{Level 3 STAMP designation,}
#'  \item{LEV4}{Level 4 STAMP designation,}
#'  \item{GROUP}{Group ID signifying group membership,}
#'  \item{AREA}{Polygon area in appropriate areal units,}
#'  \item{--}{(optional) Additional columns from directional analysis if \code{direction = TRUE},}
#'  \item{--}{(optional) Additional columns from distance analysis if \code{distance = TRUE},}
#  \item{--}{(optional) Additional columns from shape analysis if \code{shape = TRUE}.}
#'
#' @references
#'  Robertson, C., Nelson, T., Boots, B., and Wulder, M. (2007) STAMP: Spatial-temporal analysis of moving polygons.
#'  \emph{Journal of Geographical Systems}, 9:207-227.
#'
#' @keywords stamp
#' @seealso stamp.direction stamp.distance stamp.map stamp.group.summary
#' @export
#
# ---- End of Documentation ----

stamp <- function(T1, T2, dc=0, direction=FALSE, distance=FALSE, ...){ 
  
  #T1 <- fire1
  #T2 <- fire2
  # intersection b/w T1 and T2
  #row.names(T1) <- paste0("1_",row.names(T1))
  #row.names(T2) <- paste0("2_",row.names(T2))
  
  T1$id1 = row.names(T1)
  T2$id2 = row.names(T2)
  
  pI <- suppressWarnings(st_intersection(T1,T2))
  
  if (!is.null(pI)){
    pI$LEV1 <- "STBL"
    row.names(pI) <- paste0("STBL",seq(1:nrow(pI)))
  } 
  
  #T1 and T2 difference
  # A helper function that erases all of y from x:
  st_erase = function(x, y) {st_difference(x, st_union(st_combine(y)))}
  
  
  gd1 <- suppressWarnings(st_erase(T1,T2))
  gd1$LEV1 <- "DISA"
  gd1$id2 <- NA
  row.names(gd1) <- paste0("DISA",seq(1:nrow(gd1)))
  
  gd2 <- suppressWarnings(st_erase(T2,T1))
  gd2$LEV1 <- "GENR"
  gd2$id1 <- NA
  row.names(gd2) <- paste0("GENR",seq(1:nrow(gd2)))
  
  
  #Piece them together
  cols <- c('id1','id2','LEV1')
  stmp <- rbind(gd1[,cols],pI[,cols],gd2[,cols])
  
  #assign event types ---
  stmp$LEV2 <- stmp$LEV1

  #get contraction events
  id.stab1 <- unique(stmp$id1[which(stmp$LEV1 == "STBL")])
  stmp$LEV2[which(stmp$LEV1 == "DISA" & stmp$id1 %in% id.stab1)] <- "CONT"

  #get expansion events
  id.stab2 <- unique(stmp$id2[which(stmp$LEV1 == "STBL")])
  stmp$LEV2[which(stmp$LEV1 == "GENR" & stmp$id2 %in% id.stab2)] <- "EXPN"

  #Delineate contiguous bases for groups
  stmp$TMP <- 1
  if(length(stmp) > 1) {
  nbl <- poly2nb(stmp)
  for(i in 1:nrow(stmp)) {
    nbl[[i]] <- c(unlist(nbl[i]), i)
    }
  stmp$TMP <- n.comp.nb(nbl)$comp.id
  }
  #Label all other LEV2 movement types...
  gdInd <- which(stmp$LEV2 == "GENR" | stmp$LEV2 == "DISA")
  tempLev <- stmp$LEV2
  for(i in gdInd) {
    event1 <- stmp$LEV2[i]
    #find D of all appropriate polys
    dists <- vector(length=length(stmp), mode="numeric")
    dists[] <- NA
    for(j in 1:nrow(stmp)) {
      #Do not include nearest GEN-GEN or DIS-DIS as they do not change names
      if (stmp$LEV2[i] != stmp$LEV2[j]){dists[j] <- st_distance(stmp[j,], stmp[i,])}
      }
    #sort by D then extract if below dc value
    if (min(dists,na.rm=T) <= dc){
      minInd <- which(dists == min(dists, na.rm=T))[1]
      event2 <- stmp$LEV2[minInd]
      if (event1 == "DISA"){
        tempLev[i] <- switch(event2,
          GENR = "DISP1",
          EXPN = "CONV",
          CONT = "CONC",
          STBL = "CONC")
        }
      else {
        tempLev[i] <- switch(event2,
          DISA = "DISP2",
          EXPN = "FRAG",
          CONT = "DIVR",
          STBL = "CONC")
        }
      #Group movement event into appropriate contiguous group
      stmp$TMP[i] <- stmp$TMP[minInd]
      }
    }
  stmp$LEV3 <- tempLev
  #Rename groups so there are no gaps
  grps <- unique(stmp$TMP)
  for (i in 1:length(grps)){
    stmp$TMP[which(stmp$TMP == grps[i])] <- i
    }
  #Label Groups with Multi-Stable events as union or division
  stmp$LEV4 <- NA
  for (grp in unique(stmp$TMP)){
    ind <- which(stmp$TMP == grp & stmp$LEV3 == "STBL")
    ind.grp <- which(stmp$TMP == grp)
    if (length(ind) > 1){
      if (length(unique(stmp$ID2[ind])) == 1){stmp$LEV4[ind.grp] <- "UNION"}
      else if (length(unique(stmp$ID1[ind])) == 1){stmp$LEV4[ind.grp] <- "DIVISION"}
      else {stmp$LEV4[ind.grp] <- "BOTH"}
      }
    }

  #Delete TMP column and make a GROUP column
  stmp$GROUP <- stmp$TMP

  #sort by group column
  stmp <- stmp[order(stmp$GROUP),]
  #rename FID's
  #stmp <- spChFIDs(stmp,as.character(seq(0,(length(stmp)-1))))
  #Create a polygon area column
  stmp$AREA <- st_area(stmp,by_element=TRUE)
  
  #Choose columns to keep
  stmp <- stmp[,c('AREA','id1','id2','LEV1','LEV2','LEV3','LEV4','GROUP')]
  #directional analysis
  if (direction==TRUE){stmp <- stamp.direction(stmp,...)}
  #distance analysis
  if (distance==TRUE){stmp <- stamp.distance(stmp,...)}

  #output
  return(stmp)
  }
#-------------- END of stamp ---------------------------------------------------