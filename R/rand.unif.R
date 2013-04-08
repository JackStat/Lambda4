#' Generate Random Uniform [0,1] variables
#'

rand.unif<-function(n){
	replicate(n, wich.hill())
}