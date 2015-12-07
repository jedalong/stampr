# ---- roxygen documentation ----
#
#' @title Spatial temporal analysis of moving polygons
#'
#' @description
#' This function generates a \code{SpatialPolygonsDataFrame} that can be used for spatial temporal analysis of moving polygons
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
#'          stable event in a group. Groups with one or no stable events recieve an \code{NA}
#'          value for LEV4. \cr
#'  See Robertson et al. (2007; especially Figure 1) for complete descriptions of all STAMP movement
#'  event types.
#'
#' @param T1 a \code{SpatialPolygons} object of polygons from time 1.
#' @param T2 a \code{SpatialPolygons} object of polygons from time 2.
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
#'  This function returns a \code{SpatialPolygonsDataFrame} with the following data columns:
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
#' @seealso stamp.direction stamp.distance stamp.shape stamp.map stamp.group.summary
#' @export
#
# ---- End of Documentation ----

stamp <- function(T1, T2, dc=0, direction=FALSE, distance=FALSE, ...){ 
  # intersection b/w T1 and T2
  row.names(T1) <- as.character(T1$ID)
  row.names(T2) <- as.character(T2$ID)
  pI <- gIntersection(T1,T2,byid=TRUE,drop_lower_td=TRUE)
  if (!is.null(pI)){
    #this assumes row numbers are numeric
    dfI <- data.frame(matrix(as.numeric(unlist(sapply(row.names(pI),strsplit, " "))),ncol=2, byrow=TRUE))
    names(dfI) <- c("ID1","ID2")
    #dfI <- dfI[complete.cases(dfI),]
    pI <- SpatialPolygonsDataFrame(pI,data=dfI,match.ID=FALSE)
    pI$LEV1 <- "STBL"
    row.names(pI) <- paste("STBL",seq(1:length(pI)),sep="")
  } 
  
  #T1 difference
  res <- vector(mode="list", length=length(slot(T1, "polygons")))
  dfD1 <- data.frame(ID1 = rep(NA,length(T1)),ID2 = rep(NA,length(T1)))
  #This is slow, can we improve?
  for (i in seq(along=res)) {
    gd <- gDifference(T1[i,],T2,drop_lower_td=TRUE)
    res[[i]] <- gd 
    if (!is.null(gd)){                                          
      row.names(res[[i]]) <- paste(i, row.names(res[[i]]), sep="_")    #I don't know what exactly this does?
      dfD1[i,1] <- as.numeric(row.names(T1[i,]))
    }
  }
  #Get rid of problem scenarios
  ind <- which(is.na(dfD1$ID1) & is.na(dfD1$ID2))
  if (length(ind) > 0){
    dfD1 <- dfD1[-ind,]
  }
  
  res1 <- res[!sapply(res, is.null)]
  pD1 <- NULL
  if (length(res1)>0){
    out1 <- do.call("rbind", res1)
    pD1 <- SpatialPolygonsDataFrame(as(out1,"SpatialPolygons"), data=dfD1, match.ID = FALSE)
    pD1$LEV1 <- "DISA"
    row.names(pD1) <- paste("DISA",seq(1:length(pD1)),sep="")
  }
  
  #T2 difference
  res <- vector(mode="list", length=length(slot(T2, "polygons")))
  dfD2 <- data.frame(ID1 = rep(NA,length(T2)),ID2 = rep(NA,length(T2)))
  #This is slow, can we improve?
  for (i in seq(along=res)) {
    gd <- gDifference(T2[i,],T1,drop_lower_td=TRUE)
    res[[i]] <- gd
    if (!is.null(gd)){                                          
      row.names(res[[i]]) <- paste(i, row.names(res[[i]]), sep="_")    #I don't know what exactly this does?
      dfD2[i,2] <- as.numeric(row.names(T2[i,]))
    }
  }
  #Get rid of problem scenarios
  ind <- which(is.na(dfD2$ID1) & is.na(dfD2$ID2))
  if (length(ind) > 0){
    dfD2 <- dfD2[-ind,]
  }
  
  
  res1 <- res[!sapply(res, is.null)]
  pD2 <- NULL
  if (length(res1)>0){
    out1 <- do.call("rbind", res1)
    pD2 <- SpatialPolygonsDataFrame(as(out1,"SpatialPolygons"), data=dfD2, match.ID = FALSE)
    pD2$LEV1 <- "GENR"
    row.names(pD2) <- paste("GENR",seq(1:length(pD2)),sep="")
  }
  
  #Piece them together
  stmp <- do.call('rbind',c(pD1,pI,pD2))
  
  #assign event types ---
  stmp$LEV2 <- stmp$LEV1

  #get contraction events
  id.stab1 <- unique(stmp$ID1[which(stmp$LEV1 == "STBL")])
  stmp$LEV2[which(stmp$LEV1 == "DISA" & stmp$ID1 %in% id.stab1)] <- "CONT"

  #get expansion events
  id.stab2 <- unique(stmp$ID2[which(stmp$LEV1 == "STBL")])
  stmp$LEV2[which(stmp$LEV1 == "GENR" & stmp$ID2 %in% id.stab2)] <- "EXPN"

#Delineate contiguous bases for groups
  nbl <- poly2nb(stmp)
  for(i in 1:length(stmp)) {
    nbl[[i]] <- c(unlist(nbl[i]), i)
    }
  stmp$TMP <- n.comp.nb(nbl)$comp.id

  #Label all other LEV2 movement types...
  gdInd <- which(stmp$LEV2 == "GENR" | stmp$LEV2 == "DISA")
  tempLev <- stmp$LEV2
  for(i in gdInd) {
    event1 <- stmp$LEV2[i]
    #find D of all appropriate polys
    dists <- vector(length=length(stmp), mode="numeric")
    dists[] <- NA
    for(j in 1:length(stmp)) {
      #Do not include nearest GEN-GEN or DIS-DIS as they do not change names
      if (stmp$LEV2[i] != stmp$LEV2[j]){dists[j] <- gDistance(stmp[j,], stmp[i,])}
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
  stmp$LEV4 <- 'N/A'
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
  stmp@data <- stmp@data[,-5]
  #sort by group column
  stmp <- stmp[order(stmp$GROUP),]
  #rename FID's - This can still throw an error...
  stmp <- spChFIDs(stmp,as.character(seq(0,(length(stmp)-1))))
  #Create a polygon area column
  stmp$AREA <- gArea(stmp,byid=TRUE)

  #directional analysis
  if (direction==TRUE){stmp <- stamp.direction(stmp,...)}
  #distance analysis
  if (distance==TRUE){stmp <- stamp.distance(stmp,...)}
  #Shape analysis
  #if (shape==TRUE){stmp <- stamp.shape(stmp,...)}

  #output
  return(stmp)
  }
#-------------- END of stamp ---------------------------------------------------