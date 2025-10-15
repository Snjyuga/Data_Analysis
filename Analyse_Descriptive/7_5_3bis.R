#===================================
# Fontion utile au script principal
#===================================

#-------------------------------------
# fonction pour calculer la population
#-------------------------------------

get_population <- function(Effectifs){
	total<- 0 
	n<-length(Effectifs)
	i <- 1
	for (i in 1:n){
		total<- total +Effectifs[i]
	}
	return(total)
}

#-------------------------------------
#fonction pour calculer les EffectifsCC
#--------------------------------------

get_effectifs_cumulees <- function(Effectifs){
	n<- length(Effectifs)
	Effectifs_cc<- rep(0,time= n)
	Effectifs_cc[1]<-Effectifs[1]
	for(i in 2:n){
	Effectifs_cc[i]<-Effectifs_cc[i-1]+Effectifs[i]
	}
	return(Effectifs_cc)
}


#------------------------------------------------
# fonction pour calculer les frequences relatives
#-------------------------------------------------

get_frequences_rel <- function(Effectifs,Population){
	n <- length(Effectifs)
	frequences<- rep(0,times=n)
	for(i in 1:n){
	frequences[i] <- round(Effectifs[i]/Population, 4)
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

#----------------------------------------------------------------------------------
# fonction qui me retourne la proportion,d'employés qui perçoivent au moins 400.000 F CFA
#------------------------------------------------------------------------------------------

get_proportion <-  function(Bornes_inf,Frequences){
	n <- length(Frequences)
	i <- 1
	proportion <- 0
	for (i in 1:n){
		if (Bornes_inf[i] >= 40){
			 proportion <- proportion + Frequences[i]

		}
	}
	proportion <- proportion * 100
	return (proportion)
}





