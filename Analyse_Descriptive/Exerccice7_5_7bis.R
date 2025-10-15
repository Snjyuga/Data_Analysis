#===================================
# Fontion utile au script principal
#===================================

#-------------------------------------
# fonction pour calculer la population
#-------------------------------------

get_population <- function(NombreD_avis){
	total <- 0 
	n <- length(NombreD_avis)
	i <- 1
	for (i in 1:n){
		total <- total + NombreD_avis[i]
	}
	return(total)
}

#-------------------------------------
#fonction pour calculer les EffectifsCC
#--------------------------------------

get_effectifs_cumulees <- function(NombreD_avis){
	n <- length(NombreD_avis)
	Effectifs_cc <- rep(0,time= n)
	Effectifs_cc[1] <- NombreD_avis[1]
	for(i in 2:n){
	Effectifs_cc[i] <- Effectifs_cc[i-1] + NombreD_avis[i]
	}
	return(Effectifs_cc)
}

#------------------------------------------------
# fonction pour calculer les frequences relatives
#-------------------------------------------------

get_frequences_rel <- function(NombreD_avis,Population){
	n <- length(NombreD_avis)
	frequences<- rep(0,times=n)
	for(i in 1:n){
	frequences[i] <- round(NombreD_avis[i]/Population, 4)
	}
	return(frequences)
}

#----------------------------------------
# fonction pour calculer les frequencesCC
#-----------------------------------------
get_frequences_cc <- function(Frequences){
	n <- length(Frequences)
	frequencescc <- rep(0,times=n)
	frequencescc[1] <- Frequences[1]
	for(i in 2:n){frequencescc[i] <- frequencescc[i-1] + Frequences[i]}
	return(frequencescc)
}

#----------------------------------------
# fonctions calcul des proportions d'avis
#-----------------------------------------

get_proportion_a <- function(NombreD_etoiles,FrequencesCC){
	i <- 1
	for(i in 1:length(NombreD_etoiles)){
		if(NombreD_etoiles[i] == 2){
			a <- FrequencesCC[i-1]*100
		}
	}
	proportion_a <- paste("La proportion d'avis notés d'au plus 2 étoiles est de : ", a,"%.")
	return(proportion_a)
}

get_proportion_b <- function(NombreD_etoiles,Frequences){
	i <- 1
	n <- length(NombreD_etoiles)
	for(i in 1:n){
		if(NombreD_etoiles[i] == 3){
			b <- (1 - FrequencesCC[i])*100
		}
	}
	proportion_b <- paste("La proportion d'avis notés d'au moins 3 étoiles est de : " , b, " % . " )
	return(proportion_b)
}

get_proportion_c <- function(NombreD_etoiles,Frequences){
	i <- 1
	n <- length(NombreD_etoiles)
	c <- 0
	for(i in 1:n){
		if(NombreD_etoiles[i] > 1 && NombreD_etoiles[i] < 4){
		c <- c + Frequences[i]
		}
	}
	c <- c *100
	proportion_c <- paste("La proportion d'avis notés entre 1 et 4 étoiles est de : " , c, " % . " )
	return(proportion_c)
} 

#-----------------------------------------------------
#fonction calcul des quartiles et commente la symétrie
#-----------------------------------------------------

get_quartiles <- function(NombreD_etoiles,EffectifsCC,Population){
	pos_q1 <- Population/4
	pos_q2 <- Population/2
	pos_q3 <- 3*Population/4
	position <- c(pos_q1,pos_q2,pos_q3)
	Q <- seq(from = 0, to =length(position) )
	j <- 1
	for(j in 1:length(position)){
		i <- 1
		n <- length(NombreD_etoiles)
		while(i <= n){
			if(EffectifsCC[i] >= position[j]){
				Q[j] <- NombreD_etoiles[i]
				break
			}
			i <- i+1
		
		}
	}
	return(Q)
}



get_coef_yule <- function (Quartiles){
	Q1 <- Quartiles[1]
	Q2 <- Quartiles[2]
	Q3 <- Quartiles[3]
	C <- ((Q3 - Q2)-(Q2 - Q1))/(Q3 - Q1)
	if(C < 0){
		comment <- paste("Il y'a étalement à gauche et assymétrie à droite de la série, Coef Yule:", C)
	}
	else if (C == 0){
		comment <- paste("La distribution est parfaitement symétrique car Coef Yule =" , C)
	}
	else {
		comment <- paste("Il y'a étalement à droite et assymétrie à gauche de la série, Coef Yule:", C)
	}
	return(comment)
}

#---------------------------------
#fonction calcul de la moyenne
#----------------------------------

get_mean <- function(NombreD_etoiles,NombreD_avis,Population){
	i <- 1
	NiXi <-0
	Observations <- length(NombreD_etoiles)
	for(i in 1:Observations){
		NiXi <- NiXi + (NombreD_etoiles[i] * NombreD_avis[i])
	}
	NiXi_N <- NiXi / Population
	return(NiXi_N)
}

#------------------------------
#fonction calcul de la variance
#------------------------------

get_var <- function(NombreD_etoiles,NombreD_avis,Moyenne,Population){
	i <- 1
	NiXi2 <- 0
	Observations <- length(NombreD_etoiles)
	for(i in 1:Observations){
	NiXi2 <- NiXi2 + NombreD_avis[i] *(NombreD_etoiles[i]  - Moyenne)^2
	}
	Koening <- NiXi2 / Population
	return(Koening)
}


#------------------------------------------
# fonction calcul et commente le dispersion
#------------------------------------------

get_variation <- function(Variance,Moyenne){
	cov_var <- (sqrt(Variance) / Moyenne)*100
	if (cov_var <15){
		message <- paste("La série est homogène avec une faible disparité des valeurs de la série autour de la moyenne.")
	}
	else if (cov_var >= 15 && cov_var <= 30){
		message <- paste("La série est relativement homogène(dispersion modérée autour de la moyenne).")
	}
	else {
		message <- paste ("La série est extremement dispersée et hétérogène. La moyenne n'est pas représentative de l'ensemble des avis(les notes sont trop dispersées autour d'elle).")
	}
	return(message)
}





