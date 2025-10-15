




# 1-Découper les données en 10 classes de longueur égale et dressez le tableau des effectifs,des effectifs cumulés,des fréquences et des fréquences cumulées
		
		#Création de la matrice KilometreP pour stocker les données d'études


KilometreP<-matrix(nrow=10,ncol=10,byrow=TRUE)
KilometreP[1, ]<-c(2.14,1.98,5.56,4.42,5.04,2.25,3.95,4.14,4.29,4.02)
KilometreP[2, ]<-c(4.34,3.63,3.67,3.53,5.98,3.98,1.88,6.19,4.17,5.03)
KilometreP[3, ]<-c(3.81,3.07,4.63,2.83,7.28,4.35,5.42,4.33,4.50,4.06)
KilometreP[4, ]<-c(6.11,4.51,5.74,2.79,7.49,5.41,5.47,6.21,1.40,7.03)
KilometreP[5, ]<-c(4.24,2.78,4.68,2.48,4.92,4.42,3.97,5.34,6.86,6.42)
KilometreP[6, ]<-c(1.85,5.81,3.90,7.49,7.86,4.55,4.69,6.76,4.90,4.21)
KilometreP[7, ]<-c(3.30,2.60,3.42,6.67,5.03,4.09,5.80,5.87,5.26,4.06)
KilometreP[8, ]<-c(4.24,5.55,3.33,4.76,5.11,4.55,2.13,5.87,2.86,3.30)
KilometreP[9, ]<-c(5.09,4.07,6.75,3.26,5.05,3.29,6.69,2.87,3.22,3.67)
KilometreP[10, ]<-c(3.86,3.80,4.86,6.31,6.03,2.82,3.92,3.00,5.05,7.20)

		
		#1-1 Découpage des données en 10 classes de longueur égale(meme amplitude)
		# L'amplitude correspond au rapport entre l'étendue de KilometreP sur le nombre de classe(10)
Etendue<-max(KilometreP)- min(KilometreP)
Amplitude<-Etendue/10
Amplitude<-signif(Amplitude,2)


		# Création des 10 classes dont la borne inférieur de la première classe est min(KilometreP) et la borne supérieur de la dernière classe est max(KilometreP)


		#Création de l'objet Modalite de longueur 11 qui va contenir les bornes des classes


Modalites<-rep(c(0),11)	
Modalites[1]<-min(KilometreP)
for(i in 2:11){
Modalites[i]<-Modalites[i-1]+Amplitude}

#Je crée l'objet ModalitesClasses qui contient 10 classes de meme amplitude grace à la fonction cut

bornes<-seq(min(Modalites),max(Modalites),length.out=11)

ModalitesClasses<-cut(Modalites,breaks=bornes,include.lowest=TRUE,right=FALSE)
ModalitesClasses <- ModalitesClasses[-3]


		# 1-2 Création du vecteur Effectif qui contient les éffectifs de chaque classe

Effectif<-rep(c(0),10)

for (i in 1:10) {        
  for (j in 1:10) {      
    if (KilometreP[i,j] >= Modalites[1] && KilometreP[i,j] < Modalites[2]) 
    {Effectif[1] <- Effectif[1] + 1}   
    else if (KilometreP[i,j] >= Modalites[2] && KilometreP[i,j] < Modalites[3]) 
    {Effectif[2]<-Effectif[2]+1
    }
    else if (KilometreP[i,j] >= Modalites[3] && KilometreP[i,j] < Modalites[4]) 
    {Effectif[3]<-Effectif[3]+1
    } 
    else if (KilometreP[i,j] >= Modalites[4] && KilometreP[i,j] < Modalites[5]) 
    {Effectif[4]<-Effectif[4]+1
    }
    else if (KilometreP[i,j] >= Modalites[5] && KilometreP[i,j] < Modalites[6]) 
    {Effectif[5]<-Effectif[5]+1
    }
    else if (KilometreP[i,j] >= Modalites[6] && KilometreP[i,j] < Modalites[7]) 
    {Effectif[6]<-Effectif[6]+1
    }
    else if (KilometreP[i,j] >= Modalites[7] && KilometreP[i,j] < Modalites[8]) 
    {Effectif[7]<-Effectif[7]+1
    }
    else if (KilometreP[i,j] >= Modalites[8] && KilometreP[i,j] < Modalites[9]) 
    {Effectif[8]<-Effectif[8]+1
    }
    else if (KilometreP[i,j] >= Modalites[9] && KilometreP[i,j] < Modalites[10]) 
    {Effectif[9]<-Effectif[9]+1
    }
    else if (KilometreP[i,j] >= Modalites[10] && KilometreP[i,j] < Modalites[11])
    {Effectif[10]<-Effectif[10]+1
    }
  }
}


		# 1-3 Création du vecteur EffectifCC qui contient les éffectifs cumulés croissants


EffectifCC<-rep(c(0),10)
EffectifCC[1]<-Effectif[1]
for (i in 2:10){EffectifCC[i]<-EffectifCC[i-1]+Effectif[i]}


		#1-4 Création du vecteur Frequence qui contient les fréquences relatives 

Echantillon<-100
Frequence<-rep(c(0),10)
for (i in 1:10){Frequence[i]<-Effectif[i]/Echantillon}


		# 1-5 Création du vecteur FrequenceCC qui contient les Frequences cumulées croissantes


FrequenceCC<-rep(c(0),10)
FrequenceCC[1]<-Frequence[1]
for (i in 2:10){FrequenceCC[i]<-FrequenceCC[i-1]+Frequence[i]}


		# Création d'un data frame à partir des vecteurs ModalitesClasses,Effectif,EffectifCC,Frequence,FrequenceCC


Tableau<-data.frame(ModalitesClasses,Effectif,EffectifCC,Frequence,FrequenceCC)

		#Création de l'histogramme des FrequenceCC et la courbe cumulative CC


ModalitesClasses<-as.character(ModalitesClasses)#ModalitesClasses est de mode factor la fonction mutate() à besoin qu'il soit de mode character sinon renvoie une erreur
Test<-data.frame(ModalitesClasses,FrequenceCC)


install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)


# Extraction robuste des bornes et calcul centre/largeur
Test <- Test %>%
  mutate(
    ModalitesClasses = trimws(as.character(ModalitesClasses)),
    lower = as.numeric(sub("\\[|\\(", "", sapply(strsplit(ModalitesClasses, ","), `[`, 1))),
    upper = as.numeric(sub("\\]|\\)", "", sapply(strsplit(ModalitesClasses, ","), `[`, 2))),
    centre = (lower + upper) / 2,
    largeur = upper - lower
  )

# Histogramme + polygone
ggplot(Test, aes(x = centre, y = FrequenceCC)) +
  geom_col(aes(width = largeur), fill = "skyblue", alpha = 0.7) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Histogramme et polygone des fréquences cumulées",
    x = "Classes",
    y = "Fréquence cumulée"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold")
  )









											