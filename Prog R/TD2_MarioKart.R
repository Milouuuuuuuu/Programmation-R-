# ==============================================================================
# TP2 - Rappel et cas pratique (Mario Kart Edition)
# ==============================================================================
# Adaptation du TP2 (FAO) sur le jeu de données "bodies_karts.csv"
# Objectif : Importer, explorer, filtrer, modifier et analyser les corrélations.

# ==============================================================================
# Exercice 1 - Importer les données
# ==============================================================================

# 1. Importer le jeu de données bodies_karts.csv
# On utilise le chemin relatif car le fichier a été trouvé dans le même dossier.
df <- read.csv("bodies_karts.csv", sep=";", dec=",", header = TRUE)
# Note : Si le séparateur est une virgule dans votre fichier, changez sep="," et dec="."

# Vérification du type (optionnel mais recommandé)
class(df)

# 2. Combien de karts sont présents dans ce dataset ?
nb_karts <- nrow(df)
print(paste("Nombre de karts :", nb_karts))

# 3. Affichez un résumé des données
summary(df)


# ==============================================================================
# Exercice 2 - Statistiques descriptives
# ==============================================================================

# 1. Quelle est le Poids (Weight) moyen des karts ?
# (Equivalent: Dispo_alim moyenne)
mean(df$Weight, na.rm=TRUE)

# 2. Quelle est la vitesse (Speed) cumulée de tous les karts ? (Juste pour l'exercice)
# (Equivalent: Population totale)
sum(df$Speed, na.rm=TRUE)

# 3. Quel est l'écart-type de l'Accélération ? Et du Handling ?
# (Equivalent: Ecart-type Export/Import)
sd(df$Acceleration, na.rm=TRUE)
sd(df$Handling, na.rm=TRUE)

# 4. Quelle est la médiane du Drift (Dérapage) ?
# (Equivalent: Médiane Prod_viande)
median(df$Drift, na.rm=TRUE)

# 5. Calculez les quartiles du Poids (Weight).
# (Equivalent: Quartiles Dispo_alim)
quantile(df$Weight, na.rm=TRUE)

# 6. Calculez les centiles de l'Accélération.
# (Equivalent: Centiles Import_viande)
quantile(df$Acceleration, probs = seq(0, 1, 0.01), na.rm=TRUE)


# ==============================================================================
# Exercice 3 - Tris et filtres
# ==============================================================================

# 1. Les 5 karts les plus légers (Weight petit)
# (Equivalent: 5 pays les moins peuplés)
rang_weight_asc <- order(df$Weight)
resultat_light <- head(df[ rang_weight_asc , ], n = 5)
print("Top 5 les plus légers :")
print(resultat_light[ , c("Body", "Weight")])

# 2. Les 5 karts les plus rapides (Speed grand)
# (Equivalent: 5 pays les plus peuplés)
rang_speed_desc <- order(df$Speed, decreasing = TRUE)
resultat_fast <- head(df[ rang_speed_desc , ], n = 5)
print("Top 5 les plus rapides :")
print(resultat_fast[ , c("Body", "Speed")])

# 3. Les 5 karts avec le meilleur Drift
# (Equivalent: 5 pays plus grosse Prod_viande)
rang_drift <- order(df$Drift, decreasing = TRUE)
resultat_drift <- head(df[ rang_drift , ], n = 5)
# View(resultat_drift)

# 4. Les 5 karts avec le meilleur Handling
# (Equivalent: 5 pays plus gros import)
rang_handling <- order(df$Handling, decreasing = TRUE)
resultat_handling <- head(df[ rang_handling , ], n = 5)
# View(resultat_handling)

# 5. Filtrer les karts avec un Poids >= 3
# (Equivalent: Dispo_alim >= 2300)
resultat_lourd <- subset(df, Weight >= 3)
print(paste("Nombre de karts lourds (>=3) :", nrow(resultat_lourd)))

# 6. Filtrer les karts avec Poids > 2.5 ET Accélération > 3
# (Equivalent: Dispo > 3500 & Import > 1000)
resultat_stats_hautes <- subset(df, Weight > 2.5 & Acceleration > 3)
# View(resultat_stats_hautes)

# 7. Filtrer uniquement le "Blue Falcon" et le "B Dasher"
# (Equivalent: France et Belgique)
resultat_specifique <- subset(df, Body %in% c("Blue Falcon", "B Dasher"))
print("Sélection spécifique :")
print(resultat_specifique[ , c("Body", "Speed", "Weight")])


# ==============================================================================
# Exercice 4 - Modifier le dataframe
# ==============================================================================

# 1. Ajouter une colonne 'Ratio_Accel_Weight' (Part accélération par rapport au poids)
# (Equivalent: part_export)
df$Ratio_Accel_Weight <- df$Acceleration / df$Weight

# 2. Ajouter une colonne 'Score_Total' (Produit Vitesse * Poids, exemple arbitraire)
# (Equivalent: dispo_alim_pays)
df$Score_Total <- df$Speed * df$Weight

# 3. Exporter le nouveau dataframe
write.table(x = df, file = "ExportTp2_MarioKart.csv", sep=";", row.names=FALSE)

# 4. Somme du Score Total
# (Equivalent: dispo_alim_mondiale)
score_global <- sum(df$Score_Total, na.rm=TRUE)
print(paste("Score total global :", score_global))

# 5. Question calculée : Score Global / Constante (ex: 100)
# (Equivalent: Combien d'adultes nourris)
valeur_finale <- score_global / 100
print(valeur_finale)


# ==============================================================================
# Exercice 5 - Corrélation
# ==============================================================================

# 1. Nuage de points : Weight vs Speed
# (Equivalent: Prod_viande vs Export_viande)
plot(x = df$Weight,
     y = df$Speed,
     main = "Karts : Poids vs Vitesse",
     xlab = "Poids",
     ylab = "Vitesse")

# 2. Coefficient de corrélation
cor_weight_speed <- cor(x = df$Weight, y = df$Speed)
print(paste("Corrélation Poids/Vitesse :", round(cor_weight_speed, 2)))

# 3. Matrice des corrélations (Variables quantitatives uniquement)
# On exclut la colonne 1 (Body) qui est du texte
matriceCor <- cor(df[ , -1], use = "complete.obs")
matriceCor <- round(matriceCor, 2)
print("Matrice de corrélation (arrondie) :")
print(matriceCor)

# 4. Installation de corrplot (si nécessaire)
# install.packages("corrplot")

# 5. Corrélogramme
library(corrplot)
corrplot(matriceCor, method="circle")
