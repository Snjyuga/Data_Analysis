
install.packages("plotrix") # installation de la bibliothèque plotrix afin d'utiliser la fonction pie3D
library(plotrix)

# Création de la matrice qualité du produit codé en 0,1 et 2. 0==la personne n'aime pas le produit;1==la personne aime moyennement le produit;2==la personne aime beaucoup le produit
Sondage <- matrix(nrow = 10, ncol = 10,byrow=TRUE)
Sondage[1, ] <- c(1,2,2,1,2,2,2,2,1,1);Sondage[2, ] <- c(1,0,1,2,2,0,0,1,1,1);Sondage[3, ] <- c(2,0,2,2,2,2,1,2,1,2);Sondage[4, ] <- c(1,1,2,2,0,0,2,1,2,2) ;Sondage[5, ] <- c(2,0,0,2,2,1,2,1,2,1);Sondage[6, ] <- c(2,2,1,2,1,2,2,0,2,0);Sondage[7, ] <- c(0,2,1,1,2,2,2,2,2,2);Sondage[8, ] <- c(2,2,1,2,2,1,2,2,2,2);Sondage[9, ] <- c(1,2,2,1,2,2,0,2,2,2);Sondage[10, ] <- c(1,0,2,2,2,2,1,2,2,2)

i<-1
compteur0<-0 # je crée un objet compteur0 pour stocker le nombre d'occurrence(effectif) de 0
compteur1<-0
compteur2<-0
while(i<=10){
	j<-1
	while(j<=10){
		if(Sondage[i,j]==0){
			compteur0<-compteur0+1
		}
		else if(Sondage[i,j]==1){
			compteur1<-compteur1+1
		}
		else{
			compteur2<-compteur2+1
		}
		j <- j+1
	}
	i <- i+1
}

# création de la colonne Qualité qui est la Variable étudiée ou chaque ligne correspond à une modalité
Qualite<-c("0","1","2")
Echantillon<-100
# création de la colonne Effectif ou chaque ligne donne l'effectif de la modalité correspondante et la dernière donne le total des effectifs(n de la population)  
Effectif<-c(compteur0,compteur1,compteur2)

# création de la colonne EffectifCC
effectifCC0<-compteur0
effectifCC1<-sum(compteur0,compteur1)
effectifCC2<-sum(effectifCC1,compteur2)
EffectifCC<-c(effectifCC0,effectifCC1,effectifCC2)


#Création de la colonne Fréquence
frequence0<-Effectif[1]/Echantillon
frequence1<-Effectif[2]/Echantillon
frequence2<-Effectif[3]/Echantillon
Frequence<-c(frequence0,frequence1,frequence2)

# Création de la colonne FréquenceCC
FrequenceCC0<-frequence0
FrequenceCC1<-sum(frequence0,frequence1)
FrequenceCC2<-sum(FrequenceCC1,frequence2)
FrequenceCC<-c(FrequenceCC0,FrequenceCC1,FrequenceCC2)

CaracterQ<-data.frame(Qualite,Effectif,EffectifCC,Frequence,FrequenceCC)
View(CaracterQ)
ModalitesQ<-c("n'aime pas","aime moyennement","aime")

pie3D(Effectif,labels=c(paste(ModalitesQ[1],":",Effectif[1]),paste(ModalitesQ[2],":",Effectif[2]),paste(ModalitesQ[3],":",Effectif[3])),explode=0.1,col=c("steelblue","forestgreen","orange"),labelcex=1.2,border="black",main="Distribution en fonction de l'appréciation du produit") 







# Création de la matrice prix du produit 
Sondage2 <- matrix(nrow = 10, ncol = 10,byrow=TRUE)
Sondage2[1, ] <- c(125,100,150,100,75,200,175,225,150,75);Sondage2[2, ] <- c(150,75,100,250,175,75,100,100,125,125);Sondage2[3, ] <- c(200,75,225,175,100,75,100,175,150,175);Sondage2[4, ] <- c(110,150,200,75,75,250,225,200,150,200) ;Sondage2[5, ] <- c(125,75,75,200,175,100,200,200,200,175);Sondage2[6, ] <- c(150,150,200,225,75,250,200,75,175,75);Sondage2[7, ] <- c(75,175,125,250,200,250,200,200,150,200);Sondage2[8, ] <- c(150,250,150,200,150,250,200,200,150,220);Sondage2[9, ] <- c(175,175,200,150,200,250,100,200,150,250);Sondage2[10, ] <- c(150,75,225,200,225,120,170,250,175,200)
# créer une boucle afin de parcourir la matrice et me retourner le nombre de modalité incompatible et exhaustif différente qu'il y'a dans le tableau

#Modalites2<-c()
i<-1
j<-1
for(i in 1:10){
	for(j in 1:10){
		if( !(Sondage2[i,j]) %in% (Modalites2) ){
			Modalites2 <- c(Modalites2,Sondage2[i,j])
		}
		
	}
}
#compteur<-length(Modalites2)
#print(compteur)
# calculer le nombre d'occurence(l'effectif) de chaque modalités

Modalites2<-c()
while(i<=10){
	j<-1
	while(j<=10){
		if(!Sondage2[i,j]%in%Modalites2){
			Modalites2<-c(Modalites2,Sondage2[i,j])
		}
		j<-j+1
	}
	i<-i+1
}
#compteur<-length(Modalites2)
#print(compteur)













#Création du vecteur Effectif( frequence absolu)

FrequenceA2<-table(Sondage2)

#Création du vecteur frequence relative
FrequenceR2<-prop.table(FrequenceA2)

# Création du vecteur Effectif cumulé Croissant

EffectifCC2<-rep(c(0),12)
EffectifCC2[1]<-FrequenceA2[1]
for (i in 2:12){EffectifCC2[i]<-EffectifCC2[i-1]+FrequenceA2[i]}

# Création du vecteur fréquence cumulée croissante

FrequenceCC2<-rep(c(0),12)
FrequenceCC2[1]<-FrequenceR2[1]
for(i in 2:12){FrequenceCC2[i]<-FrequenceCC2[i-1]+FrequenceR2[i]}
FrequenceCC2

#Création du tableau
CaracterPrix<-data.frame(FrequenceA2,EffectifCC2,FrequenceR2,FrequenceCC2)
CaracterPrix$Sondage2.1<-NULL
View(CaracterPrix)

