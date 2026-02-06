# TP 1 EXO 2 




print("Importation des fichiers...")

dfManga <- read.csv("C:/Users/ganda/OneDrive/Bureau/BUT SD/Prog R/manga.csv", header = TRUE, sep = ",", dec = ".")
dfAnime <- read.csv("C:/Users/ganda/OneDrive/Bureau/BUT SD/Prog R/anime.csv", header = TRUE, sep = ",", dec = ".")

# Vérification des types d'objets importés (doit être "data.frame")
print(paste("Classe de dfManga :", class(dfManga)))
print(paste("Classe de dfAnime :", class(dfAnime)))

# Visualisation (Optionnel dans un script)
# View(dfManga)
# View(dfAnime)

# 2. Dimensions des Jeux de Données

# dim() retourne un vecteur c(nombre_lignes, nombre_colonnes)
print("Dimensions Manga (Lignes, Colonnes) :")
print(dim(dfManga))

print("Dimensions Anime (Lignes, Colonnes) :")
print(dim(dfAnime))

# 3. Analyse Comparative : Moyennes des Scores


# Comparaison des moyennes de "Score"
mean_manga <- mean(dfManga$Score, na.rm = TRUE) # na.rm = TRUE pour ignorer les valeurs manquantes
mean_anime <- mean(dfAnime$Score, na.rm = TRUE)

print(paste("Moyenne Score Manga :", mean_manga))
print(paste("Moyenne Score Anime :", mean_anime))

if(mean_manga > mean_anime) {
  print("-> Les Mangas ont une moyenne plus élevée.")
} else {
  print("-> Les Animes ont une moyenne plus élevée.")
}


# 4. Analyse Comparative : Total des Votes


# Somme totale des votes
total_vote_manga <- sum(dfManga$Vote, na.rm = TRUE)
total_vote_anime <- sum(dfAnime$Vote, na.rm = TRUE)

print(paste("Total Votes Manga :", total_vote_manga))
print(paste("Total Votes Anime :", total_vote_anime))

# ------------------------------------------------------------------------------
# 5. Analyse Comparative : Homogénéité (Ecart-type)
# ------------------------------------------------------------------------------

# L'écart-type (sd) mesure la dispersion. Plus il est faible, plus c'est homogène.
sd_manga <- sd(dfManga$Score, na.rm = TRUE)
sd_anime <- sd(dfAnime$Score, na.rm = TRUE)

print(paste("Ecart-type Score Manga :", sd_manga))
print(paste("Ecart-type Score Anime :", sd_anime))

if(sd_manga < sd_anime) {
  print("-> Les notes des Mangas sont plus homogènes (écart-type plus faible).")
} else {
  print("-> Les notes des Animes sont plus homogènes.")
}

# 6. Analyse Comparative : Déciles


# Les déciles permettent de voir la répartition des notes tous les 10%.
print("Déciles des Scores Manga :")
print(quantile(dfManga$Score, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE))

print("Déciles des Scores Anime :")
print(quantile(dfAnime$Score, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE))
