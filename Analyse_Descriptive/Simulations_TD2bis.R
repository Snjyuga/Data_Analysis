
get_bernoulli <- function(){
	p <- runif(1)
	tirage <- runif(1)
	X <- 0
	if (tirage < p){
		X <- 1
	}
	else {
		X <- 0
	}
	return(X)
}


get_binomiale <- function(){
	proba_succes <- runif(1)
	tirage <- runif(3)
	n <- length(tirage)
	X <- 0
	i <- 1
	for(i in 1:n){
		if(tirage[i] < proba_succes){
			X <- X + 1		
		}
	}
	return(cat("Le nombre de succes après", n, "tirages est de :", X))
}


get_geometric <- function() {
	proba_succes <- runif(1)
	tirage <- runif(10)
	n <- length(tirage)
	X <- 0
	i <- 1
	(while i <= n){
		match(){
			X <- 
		}
	}



}















