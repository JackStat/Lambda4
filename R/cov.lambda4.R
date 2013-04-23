#' Compute Covariance Maximized Lambda4
#'
#' @param x Can be either a data matrix or a covariance matrix.
#' @param missing How to handle missing values.
#' @param show.lambda4s If TRUE then the estimates for each split are included in the output.
#' @param show.splits If TRUE then a binary matrix is exported that describes the ways the items were split.
#' @param standardize When TRUE results are standardized by using the correlation matrix instead of the covariance matrix for computation.
#' @param method Can select either Hunt or Osburn.
#' 
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export

cov.lambda4<-function (x, standardize=FALSE, missing = "complete", method="Hunt", show.lambda4s = FALSE, show.splits = FALSE) 
{
    nvar <- dim(x)[2]
    n <- dim(x)[1]
    p <- dim(x)[2]
    
    sigma <- impute.cov(x, missing)
    
    if(standardize==TRUE){
      sigma <- cov2cor(sigma)
    }
    
    sigma.split <- as.data.frame(sigma)
    sigma.split <- sigma
    sigma.split[upper.tri(sigma.split, diag = TRUE)] <- -999999
    sigma0 <- diag(sigma) - sigma 
    
    xy <- matrix(ncol = 2, nrow = nvar/2)
    for (o in 1:(nvar/2)) {
        x.m <- which(sigma.split == max(sigma.split), arr.ind = TRUE)[1,]
        xy[o, 1] <- x.m[1]
        xy[o, 2] <- x.m[2]
        sigma.split[(x.m[1]), ] <- -999999
        sigma.split[ ,(x.m[1])] <- -999999
        sigma.split[ ,(x.m[2])] <- -999999
        sigma.split[(x.m[2]), ] <- -999999
    }
    
    Ahalf <- xy[, 1]
    Bhalf <- xy[, 2]
    lftout <- which(1:nvar %in% c(Ahalf, Bhalf) == FALSE)
    if (length(c(Ahalf, Bhalf)) != nvar) {
        Bhalf <- c(Bhalf, lftout)
    }
    Ani <- length(Ahalf)
    Bni <- length(Bhalf)
    
    if(method == "Hunt"){
      Acombs <- bin.combs(Ani)
      lencombs <- nrow(Acombs)
    
      t1t.temp <- (as.numeric(1:nvar %in% Ahalf) - 0.5) * 2
      t1t.splits <- t(matrix(data = rep(t1t.temp, lencombs), nrow = nvar, 
        ncol = lencombs))
    
      full <- cbind(Acombs, Acombs)
      if (Ani != Bni) {
          full <- cbind(full, rep(1, lencombs))
      }
    
      full[, c(Ahalf, Bhalf)] <- full[, 1:nvar]
    
      if (Ani != Bni) {
          covt <- which(sigma0[lftout, ] == max(sigma0[lftout, ]))
      }
    
      if (Ani != Bni) {
          full[, lftout] <- -t1t.temp[covt]
      }
    
      t1t.matrix <- (full * t1t.splits)/2 + 0.5
      t2.matrix <- (t(t1t.matrix) - 1) * -1
      onerow <- matrix(rep(1, nvar), nrow=1)
      onevector <- t(onerow)
      l4.vect <- rep(NA, lencombs)
      for (i in 1:lencombs) {
        l4.vect[i] <- (4 * (t1t.matrix[i, ] %*% sigma %*% t2.matrix[,i]))/(onerow %*% sigma) %*% onevector
      }
      
      sl4 <- sort(l4.vect)
      Min.Split <- t1t.matrix[which(l4.vect == sl4[1]), ]
      Median.Split <- t1t.matrix[which(l4.vect == sl4[round(lencombs/2)]), ]
      Max.Split <- t1t.matrix[which(l4.vect == sl4[lencombs]), ]
      Splits <- data.frame(Min.Split, Median.Split, Max.Split)
      
      Max <- max(l4.vect)
      Mean <- mean(l4.vect)
      Median <- median(l4.vect)
      Minimum <- min(l4.vect)
      count <- lencombs
      lambda4 <- data.frame(Mean, Max, Median, Minimum)
      
      Analysis.Details <- data.frame(nvar, count)
      
      result <- list(method = method,
                     lambda4 = lambda4, 
                     Analysis.Details = Analysis.Details, 
                     lambda4s = l4.vect, 
                     Splits = Splits, 
                     show.splits=show.splits, 
                     show.lambda4s=show.lambda4s)
    }
    
    if(method == "Osburn"){
      lencombs <- 1
      t1t <- matrix(rep(NA, nvar), nrow=1)
      t1t[Ahalf] <- 1
      t1t[Bhalf] <- 0
      t2 <- (t(t1t) - 1) * -1
      onerow <- matrix(rep(1, nvar), nrow=1)
      onevector <- t(onerow)      
      l4 <- (4 *(t1t %*% sigma %*% t2) )/(onerow %*% sigma) %*% onevector
      Splits=t1t
      Analysis.Details <- data.frame(nvar, 1)
      result <- list(method = method,
                     Analysis.Details = Analysis.Details, 
                     l4 = l4, 
                     Splits = Splits, 
                     show.splits = TRUE)
    }

    class(result)<-c("cov.lambda4")
    return(result)

}
