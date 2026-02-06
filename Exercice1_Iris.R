
# TP1 - Exercice 1 : Utilisation d'un dataframe existant (Iris)

# Afficher le contenu du jeu de données iris
# Cela affiche les données dans la console.
print(iris)

# Afficher la classe de l'objet iris
class(iris)

# Afficher le jeu de données dans un onglet séparé (très utile dans RStudio)
# View(iris) # Commenté pour éviter d'ouvrir une fenêtre si exécuté en script

# Compter le nombre de lignes (observations)
# Réponse attendue : 150
nbr_lignes <- nrow(iris) 
print(paste("Nombre de lignes :", nbr_lignes))

# Réponse attendue : 5
nbr_colonnes <- ncol(iris)
print(paste("Nombre de colonnes :", nbr_colonnes))

# Afficher le nom des colonnes (variables)
print("Noms des colonnes :") 
print(colnames(iris))

# Afficher un résumé stat complet
summary(iris)


# Sélectionner uniquement certaines colonnes par leur nom
# Ici : "Sepal.Length" et "Species"
print("Colonnes Sepal.Length et Species :")
iris[ , c("Sepal.Length", "Species")]


# Sélectionner des lignes spécifiques par leur index
# Ici : Lignes 100, 103 et 105
print("Lignes 100, 103 et 105 :")
subset_lignes_specifiques <- iris[ c(100, 103, 105), ]
print(subset_lignes_specifiques)

# Sélectionner une plage de lignes
# Ici : De la ligne 50 à 100
print("Lignes 50 à 100 (extrait) :")
subset_plage <- iris[ 50:100, ]
head(subset_plage) # Affichage partiel

# Calcul de la moyenne de Sepal.Length
# L'opérateur '$' permet d'accéder directement à une colonne !!
moyenne_sepal <- mean(iris$Sepal.Length)
print(paste("Moyenne de Sepal.Length :", moyenne_sepal))

# Calcul de la médiane de Sepal.Length (Attention : consigne demandait médiane Sepal.Width mais correction montrait Sepal.Length)

mediane_sepal <- median(iris$Sepal.Length)
print(paste("Médiane de Sepal.Length :", mediane_sepal))

# Calcul de l'écart-type (Standard Deviation) de Petal.Length
ecart_type_petal <- sd(iris$Petal.Length)
print(paste("Ecart-type de Petal.Length :", ecart_type_petal))

# Calcul des déciles de Petal.Width
# seq(0.1, 0.9, 0.1) génère une séquence 0.1, 0.2, ... 0.9
deciles <- quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by = 0.1))
print("Déciles de Petal.Width :")
print(deciles)
