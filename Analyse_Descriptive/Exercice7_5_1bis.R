#===================================
# Fontion utile au script principal
#===================================

#-------------------------------------
# fonction pour calculer la population
#-------------------------------------

#'
#' Cette fonction prend un vecteur en entrée et renvoie la somme totale.
#'
#' @param vecteur Un vecteur numérique
#'
#' @return entier Un entier représentant la somme des valeurs du vecteur
#'
#' @examples
#'get_population(c(4,9,5))
#' # [1] 18
#'
get_population <- function(vecteur){
	# Vérification des entrées
	if (length(vecteur) == 0) {
		stop("Le vecteur d'entrée est vide. Impossible de calculer les fréquences.")
  	}
  
  	if (!is.numeric(vecteur)) {
    		stop("Le vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur < 0)) {
    		warning("Certaines valeurs du vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	total <- 0 
	n <- length(vecteur)
	i <- 1
	for (i in 1:n){
		total <- total + vecteur[i]
	}
	return(total)
}

#------------------------------------------------
# fonction pour calculer les frequences relatives
#-------------------------------------------------

#'
#' Cette fonction prend un vecteur en entrée et renvoie les fréquences relatives.
#' Si l'argument 'Population' n'est pas fourni, la fonction la calcule automatiquement.
#'
#' @param vecteur Un vecteur numérique 
#' @param entier Un entier (somme des valeurs du vecteur d'entrée). Si NULL, la fonction le calcule automatiquement.
#'
#' @return vecteur Un vecteur numérique des fréquences relatives
#'
#' @examples
#' get_frequences_rel(c(10,27,98))
#' # [1] 0.07 0.2 0.73 
#'
get_frequences_rel <- function(vecteur,Population = NULL){
	# Vérification des entrées
	if (length(vecteur) == 0) {
		stop("Le vecteur d'entrée est vide. Impossible de calculer les fréquences.")
  	}
  
  	if (!is.numeric(vecteur)) {
    		stop("Le vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur < 0)) {
    		warning("Certaines valeurs du vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	# Population : valeur par défaut si NULL
	if (is.null(Population)) {
		Population <- sum(vecteur)
    		message("Population non fournie → calcul automatique : ", Population)
  	}
  
  	if (Population <= 0) {
    		stop("La population doit être strictement positive.")
  	}

	n <- length(vecteur)
	frequences <- numeric(n)
	for(i in 1:n){
	frequences[i] <- vecteur[i] / Population
	}
	return(frequences)
}

#-----------------------------------------------------------------------------
# fonction pour calculer les valeurs cumulées croissantes d'un vecteur numérique
#----------------------------------------------------------------------------
#' Cette fonction prend un vecteur numérique et renvoie les valeurs cumulées croissantes.
#'
#' @param vecteur Un vecteur numérique
#'
#' @return vecteur Un vecteur des sommes cumulées
#'
#' @examples
#'get_cumul_c <- (c(1,2,3))
#' # [1] 1 3 6
#'
get_cumul_c <- function(vecteur){
		# Vérification des entrées
	if (length(vecteur) == 0) {
		stop("Le vecteur d'entrée est vide. Impossible de calculer les cumuls croissants.")
  	}
  
  	if (!is.numeric(vecteur)) {
    		stop("Le vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur < 0)) {
    		warning("Certaines valeurs du vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	n <- length(vecteur)
	cumul <- numeric(n)
	cumul[1] <- vecteur[1]
	for(i in 2:n){
		cumul[i] <- cumul[i-1] + vecteur[i]
	}
	return(cumul)
}

#----------------------------------
# fonction pour calculer la moyenne
#----------------------------------
#'
#' Cette fonction prend deux vecteurs numériques et un entier en entrée et renvoie la moyenne.
#' Si l'argument 'Population' n'est pas fourni, la fonction la calcule automatiquement.
#'
#' @param vecteur Un vecteur numérique (centres des classes)
#' @param vecteur Un vecteur numérique (vecteur :Effectifs)
#' @param entier Un entier correspondant à la population
#'
#' @return réel Un réel correspondant à la moyenne arithmétique
#'
#' @examples
#' x <- c(5,10,15); y <- c(1,10,5)
#' get_mean <- (x,y)
#' # [1] 11.25
#'
get_moyenne <- function(vecteur1,vecteur2,Population = NULL){
	# Vérification des entrées
	if (length(vecteur1)!=length(vecteur2)){
		stop("Les deux vecteurs n'ont pas la meme longueur")
	}
	if (length(vecteur1) == 0) {
		stop("Le premier vecteur d'entrée est vide. Impossible de calculer la moyenne.")
  	}
  
  	if (!is.numeric(vecteur1)) {
    		stop("Le premier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur1 < 0)) {
   		warning("Certaines valeurs du premier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	if (length(vecteur2) == 0) {
		stop("Le second vecteur d'entrée est vide. Impossible de calculer la moyenne.")
  	}
  
  	if (!is.numeric(vecteur2)) {
    		stop("Le second vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur2 < 0)) {
    		warning("Certaines valeurs du second vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	# Population : valeur par défaut si NULL
	if (is.null(Population)) {
		Population <- sum(vecteur2)
    		message("Population non fournie → calcul automatique : ", Population)
  	}
  
  	if (Population <= 0) {
    		stop("La population doit être strictement positive.")
  	}

	i <- 1
	vec1_vec2 <- 0
	 n <- length(vecteur1)
	for(i in 1:n){
		vec1_vec2 <- vec1_vec2 + (vecteur1[i] * vecteur2[i])
	}
	moyenne <- vec1_vec2 / Population
	return(moyenne)
}

#------------------------------
#fonction calcul de la variance
#------------------------------
#'
#' Cette fonction prend deux vecteurs numériques(le premier correspond aux centres de classes et le second aux effectifs) 
#' et deux entier (le premier représente la moyenne et le second à la population )en entrée et renvoie la variance de la série.
#' Si l'argument 'Population' n'est pas fourni, la fonction la calcule automatiquement.
#'
#' @param vecteur Un vecteur numérique (Centre de classes)
#' @param vecteur Un vecteur numérique (Effectifs correspondants)
#' @param reel Un entier correspondant à la moyenne arithmétique de la série
#' @param entier Un entier correspondant à la population
#'
#' @return réel Un réel correspondant à la variance
#'
#' @examples
#' x <- c(5,10,15); y <- c(1,10,5); moyenne <- 3; population <- 30
#' get_var <- (x,y,moyenne,population) 
#' # [1] 5.36
#'
get_var <- function(vecteur1,vecteur2,reel = NULL,Population = NULL){
	# Vérification des entrées
	if (length(vecteur1)!=length(vecteur2)){
		stop("Les deux vecteurs n'ont pas la meme longueur")
	}
	if (length(vecteur1) == 0) {
		stop("Le premier vecteur d'entrée est vide. Impossible de calculer la variance.")
  	}
  
  	if (!is.numeric(vecteur1)) {
    		stop("Le premier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur1 < 0)) {
    		warning("Certaines valeurs du premier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	if (length(vecteur2) == 0) {
		stop("Le second vecteur d'entrée est vide. Impossible de calculer la variance.")
  	}
  
  	if (!is.numeric(vecteur2)) {
    		stop("Le second vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur2 < 0)) {
    		warning("Certaines valeurs du second vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	# Moyenne : valeur par défaut si NULL
	if (is.null(reel)){
	reel <- get_moyenne(vecteur1,vecteur2,Population)
	}
	if (reel <= 0) {
    		stop("La moyenne arithmétique doit être strictement positive.")
  	}
	# Population : valeur par défaut si NULL
	if (is.null(Population)) {
		Population <- sum(vecteur2)
    		message("Population non fournie → calcul automatique : ", Population)
  	}
  
  	if (Population <= 0) {
    		stop("La population doit être strictement positive.")
  	}
	i <- 1
	produit <- 0
	n <- length(vecteur1)
	for(i in 1:n){
	produit <- produit + vecteur2[i] *(vecteur1[i]  - reel)^2
	}
	Koening <- produit / Population
	return(Koening)
}

#---------------------------------------------------------
#Calcul de la médiane à partir des classes et FréquencesCC
#----------------------------------------------------------
#'
#' Cette fonction prend trois vecteurs numériques(le premier correspond aux bornes inférieures des classes,  
#' le second aux bornes supérieures et le dernier aux fréquences relatives)en entrée et renvoie la médiane de la série.
#'
#' @param vecteur Un vecteur numérique (Bornes inférieures des classes)
#' @param vecteur Un vecteur numérique (Bornes supérieures des classes)
#' @param vecteur Un vecteur numérique (Fréquences cumulées croissante de la série)
#'
#' @return reel Un réel correspondant à la médiane
#'
#' @examples
#' x <- c(5,10,15); y <- c(1,10,5); z <- c((5,4,3)
#' get_mediane <- (x,y,z) 
#' # [1] 5.36
#'
get_mediane <- function(vecteur1,vecteur2,vecteur3){
	# Vérification des entrées
	if (length(vecteur1)!=length(vecteur2)||length(vecteur1)!=length(vecteur3)||length(vecteur2)!=length(vecteur3)){
		stop("Les trois vecteurs n'ont pas la meme longueur")
	}
	if (length(vecteur1) == 0) {
		stop("Le premier vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur1)) {
    		stop("Le premier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur1 < 0)) {
    		warning("Certaines valeurs du premier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	if (length(vecteur2) == 0) {
		stop("Le second vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur2)) {
    		stop("Le second vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur2 < 0)) {
    		warning("Certaines valeurs du second vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	if (length(vecteur3) == 0) {
		stop("Le dernier vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur3)) {
    		stop("Le dernier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur3 < 0)) {
    		warning("Certaines valeurs du dernier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	y <- 0.5
	n <- length(FrequencesCC)
	i <- 1
	while(i <= n){
		if(FrequencesCC[i] >= y){
			y2 <- FrequencesCC[i]
			x2 <- Bornes_sup[i]
			x1 <- Bornes_inf[i]
			y1 <- FrequencesCC[i - 1]
			break
		}
		i <- i + 1
	}
	x <- x1 + (x2 - x1)*(y - y1)/(y2 - y1)
	return(x)
}

#------------------------------------------------------------------------------------------------------------------------
# fonction qui calcul le mode variable continue (classes d'amplitudes différentes)par la méthode d'interpolation linéaire
#------------------------------------------------------------------------------------------------------------------------
#'
#' Cette fonction prend trois vecteurs numériques(le premier correspond aux bornes inférieures des classes,  
#' le second aux bornes supérieures et le dernier aux densités)en entrée et renvoie le mode de la série.
#'
#' @param vecteur Un vecteur numérique (Bornes inférieures des classes)
#' @param vecteur Un vecteur numérique (Bornes supérieures des classes)
#' @param vecteur Un vecteur numérique (Densités)
#'
#' @return reel Un réel correspondant au mode
#'
#' @examples
#' x <- c(5,10,15); y <- c(1,10,5); z <- c((5,4,3)
#' get_mediane <- (x,y,z) 
#' # [1] 5.36
#'
get_mode <- function(vecteur1,vecteur2,vecteur3){
	# Vérification des entrées
	if (length(vecteur1)!=length(vecteur2)||length(vecteur1)!=length(vecteur3)||length(vecteur2)!=length(vecteur3)){
		stop("Les trois vecteurs n'ont pas la meme longueur")
	}
	if (length(vecteur1) == 0) {
		stop("Le premier vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur1)) {
    		stop("Le premier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur1 < 0)) {
    		warning("Certaines valeurs du premier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}

	if (length(vecteur2) == 0) {
		stop("Le second vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur2)) {
    		stop("Le second vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur2 < 0)) {
    		warning("Certaines valeurs du second vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	if (length(vecteur3) == 0) {
		stop("Le dernier vecteur d'entrée est vide. Impossible de calculer la médiane.")
  	}
  
  	if (!is.numeric(vecteur3)) {
    		stop("Le dernier vecteur d'entrée doit être un vecteur numérique.")
  	}
  
  	if (any(vecteur3 < 0)) {
    		warning("Certaines valeurs du dernier vecteur d'entrée sont négatifs. Vérifie tes données.")
  	}
	i <- 1
	densite <- vecteur3[1]
	densite1 <- 0
	densite2 <- 0
	borne_inf <- 0
	borne_sup <- 0
	n <- length(vecteur1)
	for (i in 2:n){
		if (vecteur3[i] > densite){
			densite <- vecteur3[i]
			densite1 <- vecteur3[i] - vecteur3[i - 1]
			densite2 <- vecteur3[i + 1] - vecteur3[i]
			borne_inf <- vecteur1[i]
			borne_sup <- vecteur2[i]
		}
	}
	Mo <- borne_inf + (borne_sup - borne_inf)*(densite1)/(densite1 + densite2)
	return(Mo)
}













