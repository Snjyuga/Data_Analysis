#Exercice1:Installation de R
#Télécharger la bibliothèque BioStatR
#install.packages("BioStatR")
#vérifier que le package à été installer
#"BioStatR" %in% rownames(installed.packages())
#help.package("BioStatR")

# Exercice 2: suites
# 1 créer la suite d'entiers consécutifs de 10 à 20
(10:20)
seq(from=10,to=20,by=1)
#2-créer la suite d'entiers en progréssion arithmétique de premier terme 20,de dernier terme 40 et de raison 5
seq(from=20,to=40,by=5)

#3-créer la suite dont tous les termes sont identiques et égaux à 28 et de longueur 10.
rep(x =28,times=10)

data(package="datasets")


(serie3<-c(T,T,F,F,T))

(v<-c(seq(102,112,1)))
w<- c(4,6,3)
(rep(x=w,times=4))
x<-rep(w,each=c(8,7,5))
x

#Exercice 2.2:
# 1- saisir la variable masse contenant 15 valeurs
masse<-c(28,27,5,27,28,30,5,30,31,29,5,30,31,31,31,5,32,30,30,5)
masse1<-c(40,39,41,37,5,43)
(nouvelle.masse<-c(masse1,masse[c(10:20)]))







