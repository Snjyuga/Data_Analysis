#Femme<-scan(file = "",what = integer(),n=1)  #Création de l'objet Femme
#Homme<-scan(file = "",what = integer(),n=1) #Création de l'objet Homme
Population<-sum(Femme,Homme)                #Création de l'objet Population à partir des objets Femme et Homme
Sex_ratio<-Homme/Femme                      # Création de l'objet Sex_ratio et calcul du rapport Homme/Femme
Deces_Femme<-236;Deces_Homme<-328
T_mortalite_F1000<-1000*Deces_Femme/Femme   # Calcul du taux de mortalité de femme pour 1000
T_mortalite_H1000<-1000*Deces_Homme/Homme         # Calcul du taux de mortalité d'homme pour 1000
T_mortalite_P1000<-1000*(Deces_Homme+Deces_Femme)/Population #Calcul du taux de mortalité pour 1000 de la population

# AFFICHAGE DES DIFFERENTS OBJETS
cat(print("La population de femme est de:"),Femme)

cat(print("La population d'homme est de:"),Homme)

cat(print("La population est de:"),Population)

cat(print("Le nombre d'homme par femme est de:"),Sex_ratio)

cat(print("Le taux de mortalité pour 1000 femme est de:"),T_mortalite_F1000)

cat(print("Le taux de mortalité pour 1000 homme est de:"),T_mortalite_H1000)

cat(print("Le taux de mortalité pour 1000 de la population est de:"),T_mortalite_P1000)

