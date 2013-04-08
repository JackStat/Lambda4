
labelled_output <- function(label, digits = 3){
    function(x, ...){
        cat(paste0(label, ":\n"))
        cat(round(x[[1]], getOption('lambda.digits', digits)), "\n")	
    }
}

#' @S3method print angoff
print.angoff<-function(x, ...){
	cat("Angoff's Coefficient \n")
	cat(round(x[[1]], getOption('lambda.digits', 3)))
	cat("\n\nSplit \n")
	cat(x[[2]])
}

#' @S3method print feldt
print.feldt<-labelled_output("Feldt's Coefficient")

#' @S3method print kristof 
print.kristof<-labelled_output("Kristof's Coefficient")

#' @S3method print lambda1
print.lambda1<-labelled_output("Guttman's Lambda 1 Coefficient")

#' @S3method print lambda2
print.lambda2<-labelled_output("Guttman's Lambda 2 Coefficient")

#' @S3method print lambda5
print.lambda5<-labelled_output("Guttman's Lambda 5 Coefficient")

#' @S3method print lambda6
print.lambda6<-labelled_output("Guttman's Lambda 6 Coefficient")

#' @S3method print omega.tot
print.omega.tot<-labelled_output("McDonald's Omega")

#' @S3method print raju
print.raju<-labelled_output("Raju's Coefficient")
