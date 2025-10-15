#---------------------------------------------------
# fonction qui calcule et stock les issues de la v.a
#----------------------------------------------------

get_resultats <- function(Die1,Die2,Table_Resultats,Lignes,Colonnes){
	Table_Resultats <- matrix(ncol= Colonnes , nrow= Lignes, byrow= TRUE)
	i <- 1
	j <- 1
	for (i in 1:Lignes){
		for (j in 1:Colonnes){
		Table_Resultats[i,j] <- Die1[i]+Die2[j]
		}
	}
	return(Table_Resultats)
}

#--------------------------------------------------------------------------------
# fonction qui parcours la matrice des issues de la v.a et stock les singletons
#-------------------------------------------------------------------------------

get_univers <- function(Xi, Lignes, Colonnes, Table_Resultats){
	Xi <- c()   
	i <- 1
	while(i <= Lignes){
		j <- 1
		while(j<= Colonnes){
			if (!(Table_Resultats[i,j]) %in% Xi){
				Xi <- c(Xi, Table_Resultats[i,j])
			}
			j <- j+1
		}
		i <- i+1
	}
	return(Xi)
}

#-------------------------------------
#fonction pour calculer les Pi
#--------------------------------------


get_probas <- function(Pi, Xi, Lignes, Colonnes, Table_Resultats){
	Pi <- rep(0, times= (length(Xi)))   
	k <- 1   
	i <- 1
	while(i <= Lignes){
		j <- 1
		while(j<= Colonnes){
			for (k in 1:(length(Xi))){
				if (Table_Resultats[i,j] == Xi[k]){
					Pi[k] <- Pi[k] +1
				}
			}
			j <- j+1
		}
		i <- i+1
	}
	Pi <- Pi / length(Table_Resultats)
	Pi <- round(Pi,2)
	return(Pi)
}







