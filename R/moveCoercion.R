#' @import methods sp spacetime trajectories 
NULL

#

#' @importClassesFrom move Move MoveStack MoveBurst
NULL



#-------------------------------------------------------------------------------
# Create Track object from object from package move (from) relating to
# a given index vector (i) (internal function).
#-------------------------------------------------------------------------------
.indexVecToTrack <- function(i, from) {
  # sp row.names anpassen???: row.names(sp) <- paste(x, "_sp_", row.names(sp), sep="")
  sp <- geometry(from)[i]
  t <- slot(from, "timestamps")[i]
  # data row.names anpassen???: row.names(d) <- paste(x, "_data_", row.names(d), sep="")
  d <- slot(from, "data")[i, ]
  Track(STIDF(sp, t, d))
}


#-------------------------------------------------------------------------------
# Coerce Move object to Track object
#-------------------------------------------------------------------------------
setAs("Move", "Track",
      function(from) {
        Track(STIDF(geometry(from), from@timestamps, from@data))
      }
)


#-------------------------------------------------------------------------------
# Coerce MoveStack object to Tracks object
#-------------------------------------------------------------------------------
setAs("MoveStack", "Tracks", 
      function(from) {
        
        idList <- as.list(as.character(unique(from@trackId)))
        
        # for development only
        #x <- "Moteado"

        indexList <- lapply(idList, function(x) which(from@trackId == x))
        l <- lapply(indexList, function(x) .indexVecToTrack(x, from = from))
        
        names(l) <- paste("Track", 1:length(l), "_", idList, sep="")
                
        #avoid more levels than the real existing ones
        idData_adj <- as.data.frame(lapply(from@idData, function(z) {
          if (is.factor(z)) {
            z <- factor(z)
            } else {z}
          }))
        
        #?nötig? --> ne, aber komfortable !?: Vergebn adäquater rownames für tracksCollData
        if (nrow(idData_adj) == length(idList)) {
          row.names(idData_adj) <- names(l)
        }
        
        Tracks(l, tracksData = idData_adj)
        
      }
)