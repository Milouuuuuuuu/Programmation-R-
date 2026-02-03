
# TP1 - Exercice 3 (Partie 2 & 3) : Filtres, Fusion et Export

dfManga <- read.csv("C:/Users/ganda/OneDrive/Bureau/BUT SD/Prog R/manga.csv", header = TRUE, sep = ",", dec = ".")
dfAnime <- read.csv("C:/Users/ganda/OneDrive/Bureau/BUT SD/Prog R/anime.csv", header = TRUE, sep = ",", dec = ".")


print("--- Filtres Mangas ---")

# 1. 
# On utilise subset(dataframe, condition)
# On stocke le résultat dans un nouvel objet pour ne pas écraser l'original.
manga_top_score <- subset(dfManga, Score > 9)
print(paste("Nb Mangas > 9/10 :", nrow(manga_top_score)))

# 2. Mangas avec Votes >= 200,000
manga_popular <- subset(dfManga, Vote >= 200000)
print(paste("Nb Mangas >= 200k votes :", nrow(manga_popular)))

# 3. Mangas avec Votes >= 200,000 ET Score >= 8 (Conditions multiples avec '&')
manga_top_popular <- subset(dfManga, Vote >= 200000 & Score >= 8)
print(paste("Nb Mangas Populaires & Bien notés :", nrow(manga_top_popular)))

# 4. Mangas avec Score entre 7 et 8 inclus
manga_medium <- subset(dfManga, Score >= 7 & Score <= 8)
print(paste("Nb Mangas Score [7, 8] :", nrow(manga_medium)))


# 2. Filtres et Analyse sur les Animes

print("--- Analyse & Filtres Animes ---")

# 1. Distribution de la variable 'Rating' (Classification par âge)
# table() compte les occurrences de chaque modalité
effectif_rating <- table(dfAnime$Rating)
print("Répartition des Ratings (Effectifs) :")
print(effectif_rating)

# Nombre de modalités (catégories) différentes
print(paste("Nombre de catégories de Rating :", length(effectif_rating)))

# Pourcentage de chaque catégorie avec prop.table()
print("Répartition des Ratings (Pourcentages) :")
print(prop.table(effectif_rating) * 100) # *100 pour avoir en %

# 2. Animes classés "R - 17+ (violence & profanity)"
anime_violence <- subset(dfAnime, Rating == "R - 17+ (violence & profanity)")
print(paste("Nb Animes 'R - 17+' :", nrow(anime_violence)))

# 3. Animes "R - 17+" ET Score >= 8
anime_violence_good <- subset(dfAnime, Rating == "R - 17+ (violence & profanity)" & Score >= 8)
print(paste("Nb Animes 'R - 17+' & Score >= 8 :", nrow(anime_violence_good)))

# 4. Animes QUI NE SONT PAS "R - 17+" (Opérateur '!=')
anime_not_violence <- subset(dfAnime, Rating != "R - 17+ (violence & profanity)")
print(paste("Nb Animes SANS violence classée :", nrow(anime_not_violence)))

# 5. Animes "PG - Children" OU "G - All Ages"
# L'opérateur %in% vérifie si la valeur est dans la liste fournie.
anime_kiddos <- subset(dfAnime, Rating %in% c("PG - Children", "G - All Ages"))
print(paste("Nb Animes pour Enfants/Tous publics :", nrow(anime_kiddos)))

# 6. Animes PAS pour enfants (Négation de la condition précédente avec '!')
anime_not_kiddos <- subset(dfAnime, !Rating %in% c("PG - Children", "G - All Ages"))
print(paste("Nb Animes NON Enfants :", nrow(anime_not_kiddos)))

# 7. Animes Excellents (Score >= 9) OU Très Populaires (Vote > 400k)
# L'opérateur '|' signifie OU (l'une ou l'autre condition suffit).
anime_stars <- subset(dfAnime, Score >= 9 | Vote > 400000)
print(paste("Nb Animes Stars (Top note OU Top popularité) :", nrow(anime_stars)))


# 3. Fusion (Rebinding) et Export

print("--- Fusion et Export ---")

# 1. Harmonisation des colonnes
# Pour fusionner, il faut EXACTEMENT les mêmes colonnes. On ne garde que l'essentiel.
cols_to_keep <- c("Title", "Score", "Vote", "Ranked")
dfAnimeM <- dfAnime[ , cols_to_keep]
dfMangaM <- dfManga[ , cols_to_keep]

# 2. Ajout d'une colonne 'Type' pour distinguer l'origine après fusion
dfAnimeM$Type <- "Anime"
dfMangaM$Type <- "Manga"

# 3. Fusion verticale (ajout des lignes de l'un à la suite de l'autre) avec rbind()
# rbind = Row Bind
dfConcat <- rbind(dfMangaM, dfAnimeM)

print("Dimensions du DataFrame fusionné :")
print(dim(dfConcat))
# Vérification rapide
# View(dfConcat)

# 4. Export vers un fichier CSV
# sep = ";" : standard souvent utilisé en Europe pour Excel
# row.names = FALSE : on évite d'exporter le numéro de ligne R (inutile ici)
output_file <- "C:/Users/ganda/OneDrive/Bureau/BUT SD/Prog R/ExportTp1.csv"
write.table(x = dfConcat, file = output_file, sep = ";", row.names = FALSE)

print(paste("Fichier exporté avec succès :", output_file))
