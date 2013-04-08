#' Compute User Specified Lambda 4 (Split-Half)
#' 
#' @param x Can be either a data frame or a covariance matrix.
#' @param split.method Specify method for splitting items.
#' @param bootstrap Can bootstrap for standard errors.
#' @param B Specify how many bootstraps.
#' @param show.boots Specify if you want to the bootstraps included in the output.
#' @param item.stats If TRUE then item statistics are provided in the output.
#' @param missing How to handle missing values.
#' 
#' @references
#' Guttman L (1945). "A Basis for Analyzing Test-Retest Reliability." Psychometrika, 10, 255-282.
#' @author Tyler Hunt \email{tyler@@psychoanalytix.com}
#' 
#' @export


user.lambda4<-function(x, split.method="even.odd", bootstrap=FALSE, B=1000, show.boots=FALSE, item.stats=FALSE, missing="complete"){

 #number of variables
 nvar<-dim(x)[2] 
 #number of participants
 n<-dim(x)[1]
 
 #Determines if x is a covariance or a data matrix and establishes a covariance matrix for estimation.
 p <- dim(x)[2]
 sigma <- impute.cov(x, missing)
 
 if(split.method[1]=="even.odd") t1t.split<-rep(c(1,0),ceiling(nvar/2))[1:nvar] 
 if(split.method[1]=="random") t1t.split<-round(runif(nvar))
 if(split.method[1]=="evenly.random") t1t.split<-sample(rep(c(1,0),ceiling(nvar/2))[1:nvar])
 if(split.method[1]==1 | split.method[1]==0) t1t.split<-split.method 
 if(length(t1t.split)!=nvar)
 	warning("The length of split is not the same as the number of items")
 Split<-t1t.split
 Obs<-colSums(!is.na(x))
 
 if(item.stats==TRUE){
 if (dim(x)[1] != p)
 	{
 	Mean<-round(colMeans(x, na.rm=TRUE),digits=2)
 	SD<-round(sapply(x,sd, na.rm=TRUE), digits=2)
 	Item.Statistics<-data.frame(Split,Mean,SD,Obs, row.names=(colnames(x)))
	 }
else
	{Item.Statistics<-data.frame(Split, Obs)}}
 t1t.split<-t(t1t.split)
 t2.split<-(t(t1t.split)-1)*-1
 
 onerow<-rep(1, nvar)
 onerow<-t(onerow)
 onevector<-t(onerow)
 if(bootstrap==TRUE){
 	temp<-rep(NA, B)
 	for(i in 1:B){
 		samp<-round(sample(1:n, replace=TRUE))
 		sigma<-cov(x[samp,], use="pairwise")
 		temp[i]<-(4*(t1t.split%*%sigma%*%t2.split))/(onerow%*%sigma)%*%onevector
 		
 	}
 	lci<-quantile(temp,.45)
 	uci<-quantile(temp,.55)
 	lambda4<-mean(temp)
 	lambda4<-data.frame(lci,lambda4,uci, row.names=NULL)
 	Boots<-temp
 }
 else {lambda4<-(4*(t1t.split%*%sigma%*%t2.split))/sum(sigma)
 	lambda4<-data.frame(lambda4)}
 	
 if(item.stats==FALSE)
 	Item.Statistics=NULL
 if(show.boots==FALSE)
 	Boots=NULL
 
 result<-list(lambda4=lambda4, Item.Statistics=Item.Statistics, Boots=Boots)
 
 class(result)=c("Lambda4", "print.user.lambda4")
 return(result)
 }

