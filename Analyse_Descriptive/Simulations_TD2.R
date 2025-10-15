Bernoulli <- get_bernoulli()
Bernoulli

Binomiale <- get_binomiale()


proba_succes <- runif(1)
proba_succes
	tirage <- runif(10)
tirage
i <- 1
n <- length(tirage)
for(i in 1:n)
index_succes1 <- tirage < proba_succes
X <- match(index_succes1,tirage)
X
  

	n <- length(tirage)
	X <- 0
	i <- 1
	(while i <= n){
		match(){
			X <-