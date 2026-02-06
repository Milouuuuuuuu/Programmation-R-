
# Exercice 1 - Importer les données


# 1. Importer le jeu de données bodies_karts.csv
# sep=";" car le fichier utilise des points-virgules
# dec="," car les décimales sont indiquées par des virgules (ex: 2,0)
df <- read.csv("C:/Users/ganda/OneDrive/Bureau/BUT SD/Prog R/bodies_karts.csv", sep=";", dec=",", header = TRUE)

# Vérification des colonnes disponibles (Crucial pour éviter l'erreur "argument 1 n'est pas un vecteur")
print("Colonnes disponibles :")
print(colnames(df))

# 2. Combien de karts sont présents dans ce dataset ?
nb_karts <- nrow(df)
print(paste("Nombre de karts :", nb_karts))

# 3. Affichez un résumé des données
summary(df)


# ==============================================================================
# Exercice 2 - Statistiques descriptives
# ==============================================================================

# 1. Quelle est le Poids (Weight) moyen des karts ?
mean(df$Weight, na.rm=TRUE)

# 2. Quelle est la vitesse au sol (Ground.Speed) cumulée de tous les karts ?
# On utilise Ground.Speed car "Speed" n'existe pas tout seul.
sum(df$Ground.Speed, na.rm=TRUE)

# 3. Quel est l'écart-type de l'Accélération ? Et du Handling (Ground.Handling) ?
sd(df$Acceleration, na.rm=TRUE)
sd(df$Ground.Handling, na.rm=TRUE)

# 4. Quelle est la médiane du Mini-Turbo ? 
# (On utilise Mini.Turbo comme proxy pour Drift ou stat fun)
median(df$Mini.Turbo, na.rm=TRUE)

# 5. Calculez les quartiles du Poids (Weight).
quantile(df$Weight, na.rm=TRUE)

# 6. Calculez les centiles de l'Accélération.
quantile(df$Acceleration, probs = seq(0, 1, 0.01), na.rm=TRUE)



# Exercice 3 - Tris et filtres


# 1. Les 5 karts les plus légers (Weight petit)
rang_weight_asc <- order(df$Weight)
resultat_light <- head(df[ rang_weight_asc , ], n = 5)
print("Top 5 les plus légers :")
print(resultat_light[ , c("Body", "Weight")])

# 2. Les 5 karts les plus rapides au sol (Ground.Speed grand)
rang_speed_desc <- order(df$Ground.Speed, decreasing = TRUE)
resultat_fast <- head(df[ rang_speed_desc , ], n = 5)
print("Top 5 les plus rapides (Sol) :")
print(resultat_fast[ , c("Body", "Ground.Speed")])

# 3. Les 5 karts avec le meilleur Mini-Turbo
rang_turbo <- order(df$Mini.Turbo, decreasing = TRUE)
resultat_turbo <- head(df[ rang_turbo , ], n = 5)
# View(resultat_turbo)

# 4. Les 5 karts avec le meilleur Handling (Ground.Handling)
rang_handling <- order(df$Ground.Handling, decreasing = TRUE)
resultat_handling <- head(df[ rang_handling , ], n = 5)
# View(resultat_handling)

# 5. Filtrer les karts avec un Poids >= 3
resultat_lourd <- subset(df, Weight >= 3)
print(paste("Nombre de karts lourds (>=3) :", nrow(resultat_lourd)))

# 6. Filtrer les karts avec Poids > 2.5 ET Accélération > 3
resultat_stats_hautes <- subset(df, Weight > 2.5 & Acceleration > 3)
# View(resultat_stats_hautes)

# 7. Filtrer uniquement le "Blue Falcon" et le "B Dasher"
resultat_specifique <- subset(df, Body %in% c("Blue Falcon", "B Dasher"))
print("Sélection spécifique :")
print(resultat_specifique[ , c("Body", "Ground.Speed", "Weight")])


# ==============================================================================
# Exercice 4 - Modifier le dataframe
# ==============================================================================

# 1. Ajouter une colonne 'Ratio_Accel_Weight'
df$Ratio_Accel_Weight <- df$Acceleration / df$Weight

# 2. Ajouter une colonne 'Score_Global' (Produit Vitesse * Poids)
df$Score_Global <- df$Ground.Speed * df$Weight

# 3. Exporter le nouveau dataframe
write.table(x = df, file = "ExportTp2_MarioKart.csv", sep=";", row.names=FALSE)

# 4. Somme du Score Global
score_global_sum <- sum(df$Score_Global, na.rm=TRUE)
print(paste("Score total global :", score_global_sum))

# 5. Question calculée : Score Global / 100
valeur_finale <- score_global_sum / 100
print(valeur_finale)



# Exercice 5 - Corrélation


# 1. Nuage de points : Weight vs Ground.Speed
plot(x = df$Weight,
     y = df$Ground.Speed,
     main = "Karts : Poids vs Vitesse (Sol)",
     xlab = "Poids",
     ylab = "Vitesse (Sol)")

# 2. Coefficient de corrélation
cor_weight_speed <- cor(x = df$Weight, y = df$Ground.Speed)
print(paste("Corrélation Poids/Vitesse :", round(cor_weight_speed, 2)))

# 3. Matrice des corrélations (Variables quantitatives uniquement)
# On exclut la colonne 1 (Body) qui est textuelle.
# On utilise use="complete.obs" pour gérer les éventuelles valeurs manquantes.
matriceCor <- cor(df[ , -1], use = "complete.obs")
matriceCor <- round(matriceCor, 2)
print("Matrice de corrélation (extrait) :")
print(head(matriceCor)) # Affiche juste le début pour ne pas encombrer

# 4. Corrélogramme (Si corrplot est installé)
# install.packages("corrplot")
library(corrplot)
corrplot(matriceCor, method="circle", type="upper")
