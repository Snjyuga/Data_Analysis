x<-c(1,3,5,2,9,19,37,45,21,61,45,9,18)
max(x)
min(x)
mean(x)
median(x)
summary(x)


Genre<-rep(c("M","F"),c(16,17))
Age<-rep(seq(10,15,0.5),3)
Matable<-data.frame(Genre,Age)# crée un dataframe de deux colonnes nommer Genre et Age, les lignes quant à elles représentent les enregistrements ou observations
#View(Matable) # permet de voir mon dataframe dans une fenetre indépendante
#str(Matable) # renvoie le nombre de variables, d'observation du dataframe
Matable_effectifs<-as.data.frame(table(Matable)) #renvoie les éffectifs de chaque case
Matable_frequences<-as.data.frame(prop.table(Matable_effectifs))



Tableau<-merge(Matable_effectifs,Matable_frequences,by=c("Genre","Age"))



