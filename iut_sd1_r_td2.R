# ==============================================================================
# TP2 - Rappel et cas pratique (Mario Kart) - Solution Complète
# ==============================================================================

# ==============================================================================
# Exercice 1 - Importer des données
# ==============================================================================

# 1. & 2. & 3. Définir le répertoire de travail
# On pointe vers le dossier où sont stockés les fichiers csv.
# Adaptez le chemin si nécessaire. Ici on suppose que le dossier "dataset" est 
# dans le répertoire courant du projet.
setwd("dataset") 
print(paste("Répertoire de travail actuel :", getwd()))


# 4. Importer les datasets avec les bons paramètres
# Pensez à toujours vérifier le séparateur (sep) et la décimale (dec) dans le fichier brut.

# bodies_karts.csv : séparateur ';', décimale ','
bodies_karts <- read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")

# tires.csv : séparateur tabulation '\t', décimale ','
tires <- read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")

# gliders.csv : séparateur pipe '|', décimale '.'
gliders <- read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")

# drivers.csv : séparateur ';', décimale ','
drivers <- read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")


# 5. Afficher les dimensions
print("Dimensions bodies_karts :")
print(dim(bodies_karts))

print("Dimensions tires :")
print(dim(tires))

print("Dimensions gliders :")
print(dim(gliders))

print("Dimensions drivers :")
print(dim(drivers))


# ==============================================================================
# Exercice 2 - Statistique
# ==============================================================================

# 1. Résumé des données
print("--- Résumé Drivers ---")
summary(drivers)
# (Faire de même pour les autres si besoin : summary(tires), etc.)


# 2. Nuage de points Drivers : Weight / Acceleration
plot(x = drivers$Weight,
     y = drivers$Acceleration, 
     main = "Drivers : Weight / Acceleration",
     xlab = "Weight",
     ylab = "Acceleration")
# Commentaire : On observe une corrélation négative (plus c'est lourd, moins ça accélère).


# 3. Coefficient de corrélation
cor_weight_accel <- cor(x = drivers$Weight, y = drivers$Acceleration)
print(paste("Corrélation Weight/Acceleration :", round(cor_weight_accel, 2)))


# 4. Calcul manuel de la corrélation (Covariance / (Ecart-type X * Ecart-type Y))
covXY <- cov(x = drivers$Weight, y = drivers$Acceleration)
sX <- sd(drivers$Weight)
sY <- sd(drivers$Acceleration)
manuel_cor <- covXY / (sX * sY)
print(paste("Corrélation calculée manuellement :", round(manuel_cor, 2)))


# 5. Coefficient de détermination (R²)
coefDeter <- cor_weight_accel^2
print(paste("Coefficient de détermination (R²) :", round(coefDeter, 2)))


# 6. Matrice des corrélations (Drivers)
# On exclut la 1ère colonne (Nom du driver)
matriceCor_drivers <- cor(drivers[ , -1])
matriceCor_drivers <- round(matriceCor_drivers, 2)
# View(matriceCor_drivers) 
print("Matrice de corrélation Drivers (extrait) :")
print(head(matriceCor_drivers))


# 7. & 8. Corrélogramme (Drivers)
# Indispensable d'installer le package si pas déjà fait : install.packages("corrplot")
if("corrplot" %in% rownames(installed.packages()) == FALSE) {
  install.packages("corrplot")
}
library(corrplot)

print("Génération du corrélogramme Drivers...")
corrplot(matriceCor_drivers, method="circle", type="upper", tl.col="black", tl.srt=45)


# 9. Corrélogrammes pour les autres datasets

# --- Tires ---
print("Corrélogramme Tires...")
# On exclut la colonne 1 "Tire"
matriceCor_tires <- round(cor(tires[ , -1]), 1)
corrplot(matriceCor_tires, method="color", type="upper", order="hclust", 
         addCoef.col = "black", tl.col="black", tl.srt=45, diag=FALSE)

# --- Bodies Karts ---
print("Corrélogramme Bodies Karts...")
# On exclut la colonne 1 "Body"
matriceCor_bodies <- round(cor(bodies_karts[ , -1]), 1)
corrplot(matriceCor_bodies, method="color", type="upper", order="hclust", 
         addCoef.col = "black", tl.col="black", tl.srt=45, diag=FALSE)

# --- Gliders ---
print("Corrélogramme Gliders...")
# On exclut la colonne 1 "Glider"
# Attention : Ground.Handling est constant (0 variance), ce qui peut provoquer une erreur/NA dans cor()
matriceCor_gliders <- round(cor(gliders[ , -1]), 1)
# On remplace les NA par 0 pour l'affichage si nécessaire, ou on retire la colonne fautive.
matriceCor_gliders[is.na(matriceCor_gliders)] <- 0 

corrplot(matriceCor_gliders, method="color", type="upper", order="hclust", 
         addCoef.col = "black", tl.col="black", tl.srt=45, diag=FALSE)


# ==============================================================================
# Exercice 3 - Manipulation de data frame
# ==============================================================================

# 1. Resultat : Driver et Weight
resultat_1 <- drivers[ , c("Driver", "Weight")]
# View(resultat_1)

# 2. Resultat : Driver et Acceleration (10 premières lignes)
resultat_2 <- drivers[ 1:10 , c("Driver", "Acceleration")]
# View(resultat_2)

# 3. Resultat : Sans colonnes 5, 7, 9
resultat_3 <- drivers[ , -c(5, 7, 9)]

# 4. Resultat : Sans Weight et Acceleration (index)
# On doit trouver les index : Weight -> 2, Acceleration -> 3
resultat_4 <- drivers[ , -c(2, 3)]

# 5. Resultat : Driver, Acceleration, Weight (ordre imposé)
resultat_5 <- drivers[ , c("Driver", "Acceleration", "Weight")]

# 6. Resultat : Drivers lignes 3, 12, 32
resultat_6 <- drivers[ c(3, 12, 32) , ]

# 7. Resultat : Drivers lignes 32, 3, 12 (ordre imposé)
resultat_7 <- drivers[ c(32, 3, 12) , ]

# 8. Tri par Poids croissant (order)
rang_weight <- order(drivers$Weight)
resultat_8 <- drivers[ rang_weight , c("Driver", "Weight")]
print("Top 5 légers :")
print(head(resultat_8, 5))

# 9. Tri par Accélération décroissante
rang_accel <- order(drivers$Acceleration, decreasing = TRUE)
resultat_9 <- drivers[ rang_accel , c("Driver", "Acceleration")]
print("Top 5 accélération :")
print(head(resultat_9, 5))

# 10. Tri double : Accélération (DESC) puis Poids (ASC)
# decreasing = c(TRUE, FALSE) permet de mixer les ordres.
rang_double <- order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE, FALSE))
resultat_10 <- drivers[ rang_double , c("Driver", "Acceleration", "Weight")]
print("Top 5 Double critère :")
print(head(resultat_10, 5))


# ==============================================================================
# Exercice 4 - GOAT (Greatest of All Time - Optimization)
# ==============================================================================
# Objectif : Trouver la meilleure combinaison pour l'accélération.

# 1. Meilleur Driver (Max Acceleration)
topDriver <- subset(x = drivers,
                    subset = Acceleration == max(Acceleration),
                    select = c("Driver", "Acceleration"))
print("Meilleur(s) Pilote(s) pour l'accélération :")
print(topDriver)

# 2. Meilleur Glider
topGlider <- subset(x = gliders,
                    subset = Acceleration == max(Acceleration),
                    select = c("Glider", "Acceleration"))
print("Meilleur(s) Aile(s) :")
print(topGlider)

# 3. Meilleur Tires
topTires <- subset(x = tires,
                   subset = Acceleration == max(Acceleration),
                   select = c("Tire", "Acceleration"))
print("Meilleur(s) Roue(s) :")
print(topTires)

# 4. Meilleur Body
topBody <- subset(x = bodies_karts,
                  subset = Acceleration == max(Acceleration),
                  select = c("Body", "Acceleration"))
print("Meilleur(s) Chassis :")
print(topBody)


print("FIN DU TP2")
