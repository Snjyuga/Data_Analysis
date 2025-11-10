# Autre méthode: Lis des grand nombres

tailles <- c(10,50,100,1000,5000)
moyenne <- numeric(length(tailles))

for(i in seq_along(tailles)){
	echantillon <- runif(tailles[i], min = 0, max = 2)
	moyenne[i] <- mean(echantillon)
}

tailles <- c(10, 50, 100, 500, 1000, 5000)

moyennes <- numeric(length(tailles))



for(i in seq_along(tailles)) {

  echantillon <- runif(tailles[i], min = 0, max = 2)

  moyennes[i] <- mean(echantillon)

}

for(i in seq_along(tailles)){
	cat(sprintf("n = %5d:moyenne = %4f\n",tailles[i],moyenne[i]))	
}

cat("\n")
plot(tailles,moyenne,
	type = "b",
     main = "Évolution de la moyenne empirique en fonction de n",
     xlab = "Taille des échantillons",
     ylab = "Moyenne des échantillons",
     col = "darkblue",
     pch = 19,
     ylim = c(0.5,1.5),
     lwd = 2)




cat("\n1.5 LOI DES GRANDS NOMBRES\n")

cat(paste(rep("-", 50), collapse = ""), "\n")



# a) Différentes valeurs de n

tailles <- c(10, 50, 100, 500, 1000, 5000)

moyennes <- numeric(length(tailles))



for(i in seq_along(tailles)) {

  echantillon <- runif(tailles[i], min = 0, max = 2)

  moyennes[i] <- mean(echantillon)

}



cat("a) Moyennes empiriques pour différentes tailles d'échantillon:\n")

for(i in seq_along(tailles)) {

  cat(sprintf("   n = %5d : Moyenne = %.4f\n", tailles[i], moyennes[i]))

}

cat("\n")

plot(tailles,moyennes,
	type = "b",
     main = "Évolution de la moyenne empirique en fonction de n",
     xlab = "Taille des échantillons",
     ylab = "Moyenne des échantillons",
     col = "darkblue",
     pch = 19,
     ylim = c(0.5,1.5),
     lwd = 2)