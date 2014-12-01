#' @import methods sp spacetime trajectories adehabitatLT
NULL

#

#-------------------------------------------------------------------------------

## "ltraj" class 
## @name ltraj-class 
## @exportClass ltraj
setOldClass("ltraj")

setAs("ltraj", "Track",
      function(from) {
        
        stopifnot(attributes(from)$typeII)
        
        stopifnot(length(from) == 1)
        
        ltrdf <- from[[1]]
        #str(ltrdf)
        #class(ltrdf) # df mit attributes (u.a. burst)
        
        thisId <- attributes(ltrdf)$id
        thisBurst <- attributes(ltrdf)$burst
        
        lengthdf <- nrow(ltrdf) #
        
        curDf <- ltrdf[!is.na(ltrdf$x) & !is.na(ltrdf$y) & !is.na(ltrdf$date), ]
        
        # Achtung, wenn ich zeilen rausgeschmeißen habe,
        # dann muss ich die connections daten neu berechnen!?!?
        if(lengthdf != nrow(curDf)){
          
          #df_orig <- ltrdf # brauch ich das??
          
          # !!! includes a recaluculaton of step characteristics!!!!!
          # ??? was bedeutet das genau???
          if (!requireNamespace("adehabitatLT", quietly = TRUE))
            stop("Package adehabitatLT (function adehabitatLT::dl) required to coerce from ltraj")
          ltr_new <- adehabitatLT::dl(curDf)
          #ltr_new <- dl(curDf)
          
          attr(ltr_new[[1]], "id") <- thisId
          
          attr(ltr_new[[1]], "burst") <- thisBurst
          
          curDf <- ltr_new[[1]]
                    
          #warning(paste("Relocations with missing coordinates have been excluded in burst ",
          #              thisBurst, ". Burst is definitely not regular (anymore).",
          #              " The step characteristics have been recalculated.\n", sep=""))
          message(paste("Information\nBurst ", thisBurst, ": Relocations with missing coordinates have been excluded.",
                        " Burst is definitely not regular (anymore).",
                        " The step characteristics have been recalculated.\n", sep=""))
        }
        
        
        crds <- curDf[,c("x","y")]
        
        time <- curDf[,"date"]
        
        dat <- curDf[,4:length(curDf)]
        
        sp <- SpatialPoints(crds)
                
        datConnLogical <- sapply(dat, function(x) {
            if ((length(which(is.na(x))) == 1 && which(is.na(x)) == nrow(curDf))
                || (length(which(is.na(x))) == 2 && which(is.na(x)) == c(1,nrow(curDf))))
                {
              TRUE
            } else { FALSE }
          })
        
        datConnNames <- names(which(datConnLogical == TRUE))
        datConn <- data.frame(dat[ , datConnNames])
        #str(datConn)
        colnames(datConn) <- datConnNames
        #str(datConn)
                
        datDataNames <- names(which(datConnLogical == FALSE))
        datData <- data.frame(dat[ , datDataNames])
        #str(datData)
        colnames(datData) <- datDataNames
        #str(datData)
        ######
        
        #require(spacetime)
        #stidf <- STIDF(sp, time, dat)
        stidf <- STIDF(sp, time, data = datData)
                
        datConnFinal <- datConn[-(nrow(datConn)), ]
        
        Track(stidf, df = datConnFinal)
        
      }
)

