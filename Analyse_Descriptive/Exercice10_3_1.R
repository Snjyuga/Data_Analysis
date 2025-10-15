#Creer un objet mesure comprenant 30 mesures répondant à une loi de distribution uniforme avec un minimum de 20 et un maximum de 35
Mesure1<-round(runif(30, min = 20, max = 35),digits = 2) #Gération de 30 nombres pseuso-aléatoire selon la loi uniforme
Mesure1
(signif(median(Mesure1), digits = 2)) # calcul de la médiane 
(signif(mean(Mesure1), digits = 2))
(signif(sd(Mesure1), digits = 2))


#Remarques:When I use signif on the object Mesure1 return intiger however round return real
# But  signif go well on median,mean and sd???


Mesure2<-round(runif(100, min = 20, max = 35),digits = 2) #Gération de 30 nombres pseuso-aléatoire selon la loi uniforme
Mesure2
signif(median(Mesure2), digits = 2) # calcul de la médiane 
signif(mean(Mesure2), digits = 2)
signif(sd(Mesure2), digits = 2) 

#Définir une fenetre graphique qui vous permettra d'insérer 3 graphiques sur deux colonnes,1 à gauche et 2 à droite et visualiser
layout(matrix(c(1,1,2,3),2,2))


# Insérer dans la première zone graphique, le graphique de type boite à moustache de l'objet Mesure.
# Nommer ce graphique "Boite à moustache de la variable Mesure" sur deux lignes 

boxplot(Mesure1, cex.axis=0.8,cex.main=0.8,main="Boite à moustache\n de la variable Mesure1",col="#FCD203");


#Dans la deuxième zone graphique, insérer l'histogramme de cet objet Mesure
#selon des classes allant 20 à 35 avec un pas de 2.5
#Nommer ce graphique "Histogramme de la variable Mesure" pour qu'il s'affiche
# sur deux lignes. Nommer l'axes des abcisses "Groupes" et l'axes des ordonnées
# "Effectifs"

Histogramme<-hist(Mesure1, breaks=seq(from=20,to=35,by=2.5),col = "skyblue",density=7, cex.axis=0.8,xlab="Groupes", cex.lab=0.8,ylab="Effectifs", cex.main=0.8,main="Histogramme\n de la variable Mesure1");




#Dans la troisième zone graphique, insérer le polygone de fréquence correspondant à la variable Mesure1 selon les groupes de l'histogramme.
#Intituler  ce graphique Polygone de fréquence de la variable Mesure1 en l'affichant sur deux lignes.
#Nommer l'axe des ordonnées Effectifs et réduire la taille de la police des axes de moitié.


Centres_deClasses<-print(Histogramme$mids)
Effectifs<-print(Histogramme$counts)
plot(Centres_deClasses,Effectifs,type="o", col="red",cex.axis=0.5,cex.lab=0.8,cex.main=0.8,main="Polygone de fréquence \n de la variable Mesure1")
  






