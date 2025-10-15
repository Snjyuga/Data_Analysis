Caracter1<-rbinom(n=110,size = 1,prob = 0.15)
Caracter1
Tableau_effectifs1<-table(Caracter1)
Tableau_frequences1<-prop.table(Tableau_effectifs1)
Table1<-data.frame(Tableau_effectifs1,Tableau_frequences1)
Table1$Caracter1.1<-NULL #supprimer la colonne 1.1
View(Table1)
x11()
barplot(Tableau_effectifs1, names.arg = c("masculin", "féminin"),ylim=c(0,100),ylab="Effectifs", xlab="Modalités",main="Sexe",col=c("blue","pink"))


Caracter2<-rbinom(n=110,size = 2,prob = 0.40)
Caracter2
Tableau_effectifs2<-table(Caracter2)
Tableau_frequences2<-prop.table(Tableau_effectifs2)
Table2<-data.frame(Tableau_effectifs2,Tableau_frequences2)
Table2$Caracter2.1<-NULL # Supprimer la colonne 2.1
View(Table2)


library(plotrix)
Modalites<-c("Primaire","Secondaire","Universitaire")
Pourcentage<-rep(0,time=3)
for(i in 1:3){Pourcentage[i]<-signif(Tableau_frequences2[i],2)*100}
x11()
pie3D(Pourcentage,labels=c(paste(Modalites[1],":",Pourcentage[1]),paste(Modalites[2],":",Pourcentage[2]),paste(Modalites[3],":",Pourcentage[3])),explode=0.1,col=c("darkblue","green","pink"),labelcex=1.2,border="black",main="Niveaux scolaires")