#Analyse exploratoire
data()
data("mtcars")
dim(mtcars)

Dataset <- mtcars
Dataset 
str(Dataset)
Moyenne_mpg <- mean(Dataset$mpg)
Moyenne_MPG
attach(Dataset)
mean(mpg)
x <- data.frame(mpg,cyl)
plot(mpg,cyl)

# Analyse bidimensionel

cor_matrix <- cor(Dataset)
cor(cor_matrix)