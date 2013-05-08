#'  Compute Feldt Coefficient
#'
#' @param x Can be either a data matrix or a covariance matrix
#' @param split.method Specify method for splitting items?
#' @param missing How to handle missing values.
#' @param standardize When TRUE Results are standardized by using the correlation matrix instead of the covariance matrix for computation.
#'
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' @export
feldt<-function(x, split.method="even.odd", missing="complete", standardize=FALSE){

 nvar <- dim(x)[2]
 n <- dim(x)[1]
 p <- dim(x)[2]
	
 sigma <- impute.cov(x, missing)
 
 if(standardize==TRUE){
   sigma <- cov2cor(sigma)
 }
 
 if(split.method[1]=="even.odd") t1t.split<-rep(c(1,0),ceiling(nvar/2))[1:nvar] 
 if(split.method[1]=="random") t1t.split<-round(runif(nvar))
 if(split.method[1]=="evenly.random") t1t.split<-sample(rep(c(1,0),ceiling(nvar/2))[1:nvar])
 if(split.method[1]==1 | split.method[1]==0) t1t.split<-split.method 
 if(length(t1t.split)!=nvar)
 	warning("The length of split is not the same as the number of items")
 Split<-t1t.split
	
 t1t.split <- (t1t.split-.5)*2
 
 sigma.a <- cov(sigma[, which(Split==0)])
 sigma.b<- cov(sigma[, which(Split==1)])
 sigma.ab<-cov(sigma.a, sigma.b)

feldt<-4*sum(sigma.ab)/(sum(sigma)-((sum(sigma.a)-sum(sigma.b))/sqrt(sum(sigma))^2))

result<-list(feldt=feldt)
class(result)<-c("feldt")
return(result)

}
