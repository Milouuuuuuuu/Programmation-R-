
# TP2 - Rappel et cas pratique (FAO) - Solution Complète


# Exo 1

# 1. Importer le jeu de données fao.csv
# On définit le répertoire de travail si nécessaire
# setwd("C:/Users/ganda/OneDrive/Bureau/BUT SD/Prog R/dataset")
setwd("dataset") 

# Lecture du fichier
# sep=";" : séparateur point-virgule
# dec="," : décimale virgule
# header=TRUE : la première ligne contient les noms de colonnes
df <- read.csv("fao.csv", sep=";", dec=",", header = TRUE)

# Vérification de la classe et des colonnes
class(df)
print("Colonnes :")
print(colnames(df))

# 2. Combien de pays sont présents dans ce dataset ?
nb_pays <- nrow(df)
print(paste("Nombre de pays :", nb_pays))

# 3. Résumé des données
print("--- Résumé FAO ---")
summary(df)


# Exercice 2 - Statistiques descriptives


# 1. Disponibilité alimentaire moyenne mondiale
mean_dispo <- mean(df$Dispo_alim, na.rm=TRUE)
print(paste("Dispo alim moyenne (Kcal/j/pers) :", round(mean_dispo, 2)))

# 2. Nombre d'habitants dans le monde (Somme de la population)
pop_totale <- sum(df$Population, na.rm=TRUE)
print(paste("Population mondiale (milliers ?) :", pop_totale))

# 3. Import et Export de viande (Ecart-type)
sd_export <- sd(df$Export_viande, na.rm=TRUE)
sd_import <- sd(df$Import_viande, na.rm=TRUE)
print(paste("Ecart-type Export viande :", round(sd_export, 2)))
print(paste("Ecart-type Import viande :", round(sd_import, 2)))

# 4. Médiane production de viande
median_prod <- median(df$Prod_viande, na.rm=TRUE)
print(paste("Médiane Prod viande :", median_prod))

# 5. Quartiles de la disponibilité alimentaire
quantiles_dispo <- quantile(df$Dispo_alim, na.rm=TRUE)
print("Quartiles Dispo alim :")
print(quantiles_dispo)

# 6. Centiles (percentiles) de l'importation de viande
centiles_import <- quantile(df$Import_viande, probs = seq(0, 1, 0.01), na.rm=TRUE)
print("Centiles Import viande (extrait) :")
print(head(centiles_import))


# Exercice 3 - Tris et filtres


# 1. Les 5 pays les moins peuplés
rang_pop_asc <- order(df$Population)
pays_moins_peuples <- head(df[ rang_pop_asc, ], n = 5)
print("Top 5 moins peuplés :")
# View(pays_moins_peuples)

# 2. Les 5 pays les plus peuplés
rang_pop_desc <- order(df$Population, decreasing = TRUE)
pays_plus_peuples <- head(df[ rang_pop_desc, ], n = 5)
print("Top 5 plus peuplés :")
# View(pays_plus_peuples)

# 3. Les 5 pays qui produisent le plus de viande
rang_prod <- order(df$Prod_viande, decreasing = TRUE)
top_prod_viande <- head(df[ rang_prod, ], n = 5)
print("Top 5 Producteurs viande :")
# View(top_prod_viande)

# 4. Les 5 pays qui importent le plus de viande
rang_import <- order(df$Import_viande, decreasing = TRUE)
top_import_viande <- head(df[ rang_import, ], n = 5)
print("Top 5 Importateurs viande :")
# View(top_import_viande)

# 5. Pays avec Dispo_alim >= 2300 Kcal
pays_dispo_ok <- subset(df, Dispo_alim >= 2300)
print(paste("Nb pays >= 2300 Kcal :", nrow(pays_dispo_ok)))

# 6. Pays avec Dispo_alim > 3500 ET Import_viande > 1000
# Condition multiple
pays_gros_mangeurs_importateurs <- subset(df, Dispo_alim > 3500 & Import_viande > 1000)
print(paste("Nb pays > 3500 Kcal & Import > 1000 :", nrow(pays_gros_mangeurs_importateurs)))

# 7. France et Belgique
# Utilisation de %in%
pays_fr_be <- subset(df, Nom %in% c("France", "Belgique"))
print("France et Belgique :")
print(pays_fr_be)


# Exo 4 - Modifier le dataframe


# 1. Ajouter la colonne 'Part_export' (Export / Production)
df$Part_export <- df$Export_viande / df$Prod_viande

# 2. Ajouter la colonne 'Dispo_alim_pays' (Dispo par personne * Population)
# La population est une estimation, vérifiez l'unité (si milliers, le total est en milliers de kcal)
df$Dispo_alim_pays <- df$Dispo_alim * df$Population

# 3. Exporter le nouveau dataframe
write.table(x = df, file = "ExportTp2_FAO.csv", sep=";", row.names=FALSE)

# 4. Somme de la disponibilité alimentaire mondiale
dispo_totale_monde <- sum(df$Dispo_alim_pays, na.rm=TRUE)
print(paste("Dispo alim mondiale totale :", dispo_totale_monde))

# 5. Combien d'adultes peut-on nourrir (besoin 2300 kcal) ?
# dispo_totale_monde est en "Kcal * nb habitant" (unité dépend de la colonne Population)
nb_adultes_nourris <- dispo_totale_monde / 2300
print(paste("Capacité nourricière (nb adultes) :", nb_adultes_nourris))


# ==============================================================================
# Exercice 5 - Corrélation
# ==============================================================================

# 1. Nuage de points : Production vs Exportation
plot(x = df$Prod_viande,
     y = df$Export_viande,
     main = "Pays : Prod_viande / Export_viande",
     xlab = "Production Viande",
     ylab = "Export Viande")
# Commentaire : On s'attend à une corrélation positive (plus on produit, plus on peut exporter).

# 2. Coefficient de corrélation
cor_prod_export <- cor(x = df$Prod_viande, y = df$Export_viande, use = "complete.obs")
print(paste("Corrélation Prod/Export :", round(cor_prod_export, 2)))

# 3. Matrice des corrélations
# On ne garde que les colonnes numériques. 
# La colonne 1 est "Nom", on l'enlève : df[ , -1]
# use = "complete.obs" pour ignorer les NA dans le calcul
matriceCor <- cor(df[ , -1], use = "complete.obs")
matriceCor <- round(matriceCor, 2)
print("Matrice de corrélation (extrait) :")
print(head(matriceCor))

# 4. Installation de corrplot (si nécessaire)
if("corrplot" %in% rownames(installed.packages()) == FALSE) {
  install.packages("corrplot")
}

# 5. Corrélogramme
library(corrplot)
corrplot(matriceCor, method="circle", type="upper", tl.col="black", tl.srt=45)

print("FIN DU TP2 (FAO)")
