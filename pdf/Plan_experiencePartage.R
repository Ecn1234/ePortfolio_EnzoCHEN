rm(list = ls())
setwd("D:/IUT 2eme annee/SAE_Plan_experience")
.libPaths("Packages")
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("scales")
library(tidyverse)
library(readxl)
library(openxlsx)
library(scales)

# ======= #
#
# Création des tables marge et base pour le redressement sur SAS
#
# ======= #

# Création de la table marge ----

# Var Cycle
# Cursus licence BUT PASS LAS	Cursus Master	Cursus Doctorat

# Var Domaine
# Sciences,	Santé,	ArtsLettrresLanguesSHS,	Droit + economie AES


table_marge <- data.frame(
  VAR      = c("cycle", "domaine"),
  N        = c(3, 4),
  mar1  = c(59.71, 21.71),
  mar2  = c(36.99, 17.10),
  mar3  = c(3.30, 34.10),
  mar4  = c(NA, 27.09)
)


write.xlsx(x = table_marge, file = "Sorties/tableMarge.xlsx")

# Création de la table base ----

dataBrut <- read_excel("Donnees/donneesPratiquesCulturelles.xlsx")



dataBrut <- dataBrut %>%
  mutate(
    cycle = case_when(
      `Dans_quel_cycle_d'études_êtes-vous_inscrit.e_en_2024-2025_?` ==
        "Cycle 1 : licence, BUT, PASS, LAS" ~ 1,
      `Dans_quel_cycle_d'études_êtes-vous_inscrit.e_en_2024-2025_?` ==
        "Cycle 2 : Master, externat" ~ 2,
      `Dans_quel_cycle_d'études_êtes-vous_inscrit.e_en_2024-2025_?` ==
        "Cycle 3 : Doctorat, internat" ~ 3,
      TRUE ~ NA_real_
    ),
    
    domaine = case_when(
      `Quel_est_votre_domaine_d'études_?` ==
        "Sciences, Technologies, Informatique" ~ 1,
      `Quel_est_votre_domaine_d'études_?` ==
        "Santé" ~ 2,
      `Quel_est_votre_domaine_d'études_?` %in%
        c("Sciences humaines et sociales",
          "Arts, Lettres et Langues") ~ 3,
      `Quel_est_votre_domaine_d'études_?` ==
        "Droit, Economie, Gestion" ~ 4,
      TRUE ~ NA_real_
    )
  )



vars_a_garder <- c(
  # Variables ID
  "ID_de_la_réponse",
  
  #Variables redressement
  "cycle",
  "domaine",
  
  # Sorties culturelles
  "Au_cours_d'une_année_(par_exemple_l'année_2024),_combien_estimez-vous_avoir_effectué_de_sorties_culturelles_de_chacun_de_ces_types_?_[Cinéma]",
  # Pratiques numériques
  "A_quelle_fréquence_pratiquez-vous_ces_activités_à_la_maison_?_[Regarder_un_film,_une_série]",
  
  
  # Sorties culturelles 
  "Au_cours_d'une_année_(par_exemple_l'année_2024),_combien_estimez-vous_avoir_effectué_de_sorties_culturelles_de_chacun_de_ces_types_?_[Concert,_opéra]",
  # Pratiques numériques
  "A_quelle_fréquence_pratiquez-vous_ces_activités_à_la_maison_?_[Ecouter_de_la_musique]",
  
  
  # Source information traditionnelle
  "Quelles_sont_vos_principales_sources_d'infomation_pour_organiser_vos_activités_culturelles_?_[Bouche-à-oreille]",
  # Source information numérique
  "Quelles_sont_vos_principales_sources_d'infomation_pour_organiser_vos_activités_culturelles_?_[Réseaux_sociaux]"
)





dataAnalyse <- dataBrut %>%
  select(all_of(vars_a_garder))



dataAnalyse <- dataBrut %>%
  select(all_of(vars_a_garder)) %>%
  rename(
    
    
    
    sortie_cinema = `Au_cours_d'une_année_(par_exemple_l'année_2024),_combien_estimez-vous_avoir_effectué_de_sorties_culturelles_de_chacun_de_ces_types_?_[Cinéma]`,
    num_film_serie = `A_quelle_fréquence_pratiquez-vous_ces_activités_à_la_maison_?_[Regarder_un_film,_une_série]`,
    
    
    sortie_concert = `Au_cours_d'une_année_(par_exemple_l'année_2024),_combien_estimez-vous_avoir_effectué_de_sorties_culturelles_de_chacun_de_ces_types_?_[Concert,_opéra]`,
    num_musique = `A_quelle_fréquence_pratiquez-vous_ces_activités_à_la_maison_?_[Ecouter_de_la_musique]`,
    
    source_bouche_oreille = `Quelles_sont_vos_principales_sources_d'infomation_pour_organiser_vos_activités_culturelles_?_[Bouche-à-oreille]`,
    source_reseaux = `Quelles_sont_vos_principales_sources_d'infomation_pour_organiser_vos_activités_culturelles_?_[Réseaux_sociaux]`
  )




lengths(dataAnalyse) # 2721
colSums(is.na(dataAnalyse))

dataAnalyse <- dataAnalyse %>% 
  drop_na()

lengths(dataAnalyse) #  1750  
colSums(is.na(dataAnalyse))



echantillonRedressement <- dataAnalyse %>% select(ID_de_la_réponse, cycle, domaine)


write.xlsx(x = echantillonRedressement, file = "Sorties/base.xlsx")




# ======= #
#
# Fin de la création des 2 tables de redressement ----
#
# ======= #




# Importation de la table avec Poids obtenue grâce à la macro calmar----

echantillonPoids <- read_excel("Donnees/BasePoidsSASv9.xlsx") %>% select(-cycle, -domaine)



dataAnalyseAvecPoids <- inner_join(echantillonPoids, dataAnalyse, by = "ID_de_la_réponse")


lengths(dataAnalyseAvecPoids)
colSums(is.na(dataAnalyseAvecPoids))
table(dataAnalyseAvecPoids$cycle)
table(dataAnalyseAvecPoids$domaine)





dataAnalyseAvecPoids <- dataAnalyseAvecPoids %>%
  mutate(cycle = case_when(
    cycle == 1 ~ "Cursus licence BUT PASS LAS",
    cycle == 2 ~ "Cursus Master",
    cycle == 3 ~ "Cursus Doctorat"
  ))



dataAnalyseAvecPoids <- dataAnalyseAvecPoids %>%
  mutate(domaine = case_when(
    domaine == 1 ~ "Sciences et technologies",
    domaine == 2 ~ "Santé",
    domaine == 3 ~ "Arts, lettres, langues et sciences humaines et sociales",
    domaine == 4 ~ "Droit, économie et administration"
  ))






colSums(is.na(dataAnalyseAvecPoids))

# Variable Poids
table(dataAnalyseAvecPoids$poids)

#Variable redressement
table(dataAnalyseAvecPoids$cycle)
prop.table(table(dataAnalyseAvecPoids$cycle))


table(dataAnalyseAvecPoids$domaine)
prop.table(table(dataAnalyseAvecPoids$domaine))

# Variable du sujet
table(dataAnalyseAvecPoids$sortie_cinema)
table(dataAnalyseAvecPoids$num_film_serie)

table(dataAnalyseAvecPoids$sortie_concert)
table(dataAnalyseAvecPoids$num_musique)

table(dataAnalyseAvecPoids$source_bouche_oreille)
table(dataAnalyseAvecPoids$source_reseaux)





# Graphique Univairée Sans Poids----
# Variable redressement



df_cycle <- dataAnalyseAvecPoids %>% 
  group_by(cycle) %>% 
  summarise(effectif = n())

df_cycle <- df_cycle %>% 
  mutate(proportion = effectif/sum(effectif))


df_cycle$cycle <- factor(
  df_cycle$cycle,
  levels = c(
    "Cursus Doctorat",
    "Cursus Master",
    "Cursus licence BUT PASS LAS"
  )
)


df_cycle


ggplot(df_cycle, aes(x = "", y = proportion, fill = cycle)) +
  geom_col(width = 1) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Répartition des cycles d'étude (sans Poids)", fill = "Cycle")





df_domaine <- dataAnalyseAvecPoids %>%
  group_by(domaine) %>%
  summarise(effectif = n() )

df_domaine <- df_domaine %>%
  mutate(proportion = effectif / sum(effectif))

df_domaine


ggplot(df_domaine, aes(x = "", y = proportion, fill = domaine)) +
  geom_col(width = 1) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Répartition des domaines d'étude (sans Poids)", fill = "Domaine")




# Variable Sujet

# Film

df_sortie_cinema <- dataAnalyseAvecPoids %>%
  count(sortie_cinema)

ggplot(df_sortie_cinema, aes(x = sortie_cinema, y = n)) +
  geom_col(fill = "darkorange") +
  theme_minimal() +
  labs(title = "Fréquence des sorties cinéma", x = NULL, y = "Effectif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_num_film_serie <- dataAnalyseAvecPoids %>%
  count(num_film_serie)

ggplot(df_num_film_serie, aes(x = num_film_serie, y = n)) +
  geom_col(fill = "purple") +
  theme_minimal() +
  labs(title = "Fréquence de visionnage films/séries", x = NULL, y = "Effectif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Musique

df_sortie_concert <- dataAnalyseAvecPoids %>%
  count(sortie_concert)

ggplot(df_sortie_concert, aes(x = sortie_concert, y = n)) +
  geom_col(fill = "forestgreen") +
  theme_minimal() +
  labs(title = "Fréquence des sorties concerts", x = NULL, y = "Effectif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



df_num_musique <- dataAnalyseAvecPoids %>%
  count(num_musique)

ggplot(df_num_musique, aes(x = num_musique, y = n)) +
  geom_col(fill = "deepskyblue") +
  theme_minimal() +
  labs(title = "Fréquence d’écoute de musique", x = NULL, y = "Effectif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# La mobilisation de l’imaginaire et de l’univers narratif

#


df_bouche_oreille <- dataAnalyseAvecPoids %>%
  group_by(source_bouche_oreille) %>%
  summarise(effectif = n())

df_bouche_oreille <- df_bouche_oreille %>%
  mutate(proportion = effectif / sum(effectif))

ggplot(df_bouche_oreille, aes(x = source_bouche_oreille, y = proportion)) +
  geom_col(fill = "pink") +
  theme_minimal() +
  labs(title = "Fréquence de l'utilisation du bouche-à-oreille comme source d'information pour organiser des activités culturelles", x = NULL, y = "Effectif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_reseaux <- dataAnalyseAvecPoids %>%
  group_by(source_reseaux) %>%
  summarise(effectif = n())

df_reseaux <- df_reseaux %>%
  mutate(proportion = effectif / sum(effectif))

ggplot(df_reseaux, aes(x = source_reseaux, y = proportion)) +
  geom_col(fill = "gold") +
  theme_minimal() +
  labs(title = "Fréquence de l’utilisation des réseaux sociaux par les étudiants pour organiser leurs activités culturelles", x = NULL, y = "Effectif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







####
#
# Graphique avec redressement avec Poids ----
# 
####

# Univariée avec Poids ----


df_cycle <- dataAnalyseAvecPoids %>%
  group_by(cycle) %>%
  summarise(effectif = sum(poids))

df_cycle <- df_cycle %>%
  mutate(proportion = effectif / sum(effectif))


df_cycle$cycle <- factor(
  df_cycle$cycle,
  levels = c(
    "Cursus Doctorat",
    "Cursus Master",
    "Cursus licence BUT PASS LAS"
  )
)


df_cycle 
53839+603492+974169



ggplot(df_cycle, aes(x = "", y = proportion, fill = cycle)) +
  geom_col(width = 1) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Répartition des cycles (pondéré)", fill = "Cycle")



df_domaine <- dataAnalyseAvecPoids %>%
  group_by(domaine) %>%
  summarise(effectif = sum(poids))

df_domaine <- df_domaine %>%
  mutate(proportion = effectif / sum(effectif))

df_domaine
556342 + 441973 + 278987 + 354199


ggplot(df_domaine, aes(x = "", y = proportion, fill = domaine)) +
  geom_col(width = 1) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Répartition des domaines (pondéré)", fill = "Domaine")




df_sortie_cinema <- dataAnalyseAvecPoids %>%
  group_by(sortie_cinema) %>%
  summarise(effectif = sum(poids))

df_sortie_cinema <- df_sortie_cinema %>%
  mutate(proportion = effectif / sum(effectif))

df_sortie_cinema
91437 + 312231 + 509340 + 272193 + 149260 + 297039




ggplot(df_sortie_cinema, aes(x = sortie_cinema, y = effectif)) +
  geom_col(fill = "darkorange") +
  geom_text(
    aes(label = round(effectif, 0)),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "Fréquence des sorties cinéma (pondéré)",
    x = NULL,
    y = "Effectif"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(df_sortie_cinema, aes(x = sortie_cinema, y = proportion * 100)) +
  geom_col(fill = "darkorange") +
  geom_text(
    aes(label = paste0(round(proportion * 100, 1), " %")),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "Fréquence des sorties cinéma (proportion pondérée)",
    x = NULL,
    y = "Pourcentage (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



df_num_film_serie <- dataAnalyseAvecPoids %>%
  group_by(num_film_serie) %>%
  summarise(effectif = sum(poids))

df_num_film_serie <- df_num_film_serie %>%
  mutate(proportion = effectif / sum(effectif))


df_num_film_serie$num_film_serie <- factor(
  df_num_film_serie$num_film_serie,
  levels = c(
    "Jamais",
    "Rarement, tous les ans",
    "Occasionnellement, tous les mois",
    "Souvent, toutes les semaines",
    "Très souvent, tous les jours"
  )
)


df_num_film_serie




ggplot(df_num_film_serie, aes(x = num_film_serie, y = effectif)) +
  geom_col(fill = "purple") +
  geom_text(
    aes(label = format(round(effectif, 0), big.mark = " ", scientific = FALSE)),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "Fréquence de visionnage de films et séries à domicile (pondéré)",
    x = NULL,
    y = "Effectif"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df_num_film_serie, aes(x = num_film_serie, y = proportion * 100)) +
  geom_col(fill = "purple") +
  geom_text(
    aes(label = paste0(round(proportion * 100, 1), " %")),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "Fréquence de visionnage de films et séries à domicile (en % pondéré)",
    x = NULL,
    y = "Pourcentage (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




df_sortie_concert <- dataAnalyseAvecPoids %>%
  group_by(sortie_concert) %>%
  summarise(effectif = sum(poids))

df_sortie_concert <- df_sortie_concert %>%
  mutate(proportion = effectif / sum(effectif))

df_sortie_concert



ggplot(df_sortie_concert, aes(x = sortie_concert, y = effectif)) +
  geom_col(fill = "forestgreen") +
  geom_text(
    aes(label = format(round(effectif, 0), big.mark = " ", scientific = FALSE)),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "Fréquence des sorties concert (pondéré)",
    x = NULL,
    y = "Effectif"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df_sortie_concert, aes(x = sortie_concert, y = proportion * 100)) +
  geom_col(fill = "forestgreen") +
  geom_text(
    aes(label = paste0(round(proportion * 100, 1), " %")),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "Fréquence des sorties concert (en % pondéré)",
    x = NULL,
    y = "Pourcentage (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






df_num_musique <- dataAnalyseAvecPoids %>%
  group_by(num_musique) %>%
  summarise(effectif = sum(poids))

df_num_musique <- df_num_musique %>%
  mutate(proportion = effectif / sum(effectif))


df_num_musique$num_musique <- factor(
  df_num_musique$num_musique,
  levels = c(
    "Jamais",
    "Rarement, tous les ans",
    "Occasionnellement, tous les mois",
    "Souvent, toutes les semaines",
    "Très souvent, tous les jours"
  )
)



df_num_musique



ggplot(df_num_musique, aes(x = num_musique, y = effectif)) +
  geom_col(fill = "deepskyblue") +
  geom_text(
    aes(label = format(round(effectif, 0), big.mark = " ", scientific = FALSE)),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "Fréquence d'écoute de musique en streaming (pondéré)",
    x = NULL,
    y = "Effectif"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df_num_musique, aes(x = num_musique, y = proportion * 100)) +
  geom_col(fill = "deepskyblue") +
  geom_text(
    aes(label = paste0(round(proportion * 100, 1), " %")),
    vjust = -0.3,
    size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "Fréquence d'écoute de musique en streaming à domicile (en % pondéré)",
    x = NULL,
    y = "Pourcentage (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



df_source_bouche_oreille <- dataAnalyseAvecPoids %>%
  group_by(source_bouche_oreille) %>%
  summarise(effectif = sum(poids))

df_source_bouche_oreille <- df_source_bouche_oreille %>%
  mutate(proportion = effectif / sum(effectif))

df_source_bouche_oreille

ggplot(df_source_bouche_oreille, aes(x = "", y = effectif, fill = source_bouche_oreille)) +
  geom_col(width = 1) +
  geom_text(
    aes(label = format(round(effectif, 0), big.mark = " ", scientific = FALSE)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  coord_polar("y") +
  theme_void() +
  labs(
    title = "Utilisation du bouche-à-oreille comme source d'information (pondéré)",
    fill = "Source"
  )



ggplot(df_source_bouche_oreille, aes(x = "", y = proportion, fill = source_bouche_oreille)) +
  geom_col(width = 1) +
  geom_text(
    aes(label = scales::percent(proportion, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  coord_polar("y") +
  theme_void() +
  labs(
    title = "Utilisation du bouche-à-oreille comme source d'information (pondéré)",
    fill = "Source"
  )







df_source_reseaux <- dataAnalyseAvecPoids %>%
  group_by(source_reseaux) %>%
  summarise(effectif = sum(poids))

df_source_reseaux <- df_source_reseaux %>%
  mutate(proportion = effectif / sum(effectif))

df_source_reseaux

ggplot(df_source_reseaux, aes(x = "", y = effectif, fill = source_reseaux)) +
  geom_col(width = 1) +
  geom_text(
    aes(label = format(round(effectif, 0), big.mark = " ", scientific = FALSE)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  coord_polar("y") +
  theme_void() +
  labs(
    title = "Utilisation des réseaux sociaux comme source d'information (pondéré)",
    fill = "Source"
  )



ggplot(df_source_reseaux, aes(x = "", y = proportion, fill = source_reseaux)) +
  geom_col(width = 1) +
  geom_text(
    aes(label = scales::percent(proportion, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  coord_polar("y") +
  theme_void() +
  labs(
    title = "Utilisation des réseaux sociaux comme source d'information (pondéré)",
    fill = "Source"
  )








# Bivariée avec Poids ----


# Comparer pratiques numériques vs traditionnelles avec Poids ----



# diagramme en barre empilées pourcentage



# ------------------------------ #
# 1 Films/Séries × Sorties cinéma
# ------------------------------ #
cross_film_cinema <- dataAnalyseAvecPoids %>%
  group_by(num_film_serie, sortie_cinema) %>%
  summarise(effectif = sum(poids), .groups = "drop") %>%
  group_by(num_film_serie) %>%
  mutate(proportion = effectif / sum(effectif)) %>%
  ungroup()


cross_film_cinema$num_film_serie <- factor(
  cross_film_cinema$num_film_serie,
  levels = c(
    "Jamais",
    "Rarement, tous les ans",
    "Occasionnellement, tous les mois",
    "Souvent, toutes les semaines",
    "Très souvent, tous les jours"
  )
)

cross_film_cinema$sortie_cinema <- factor(
  cross_film_cinema$sortie_cinema,
  levels = c(
    "Plus de 10",
    "8 à 10",
    "6 à 8",
    "3 à 5",
    "1 ou 2",
    "0"
  )
)



print(n = Inf,cross_film_cinema )


# contingence <- cross_film_cinema %>%
#   select(num_film_serie, sortie_cinema, effectif) %>%
#   pivot_wider(names_from = sortie_cinema, values_from = effectif) %>%
#   column_to_rownames(var = "num_film_serie") %>%
#   as.matrix()
# 
# contingence
# chisq.test(contingence,correct=F)$expected
# chisq.test(contingence,correct=F)
# 
# table(dataAnalyseAvecPoids$num_film_serie,dataAnalyseAvecPoids$sortie_cinema)
# chisq.test(table(dataAnalyseAvecPoids$num_film_serie,dataAnalyseAvecPoids$sortie_cinema),correct=F)$expected
# chisq.test(table(dataAnalyseAvecPoids$num_film_serie,dataAnalyseAvecPoids$sortie_cinema),correct=F)

ggplot(cross_film_cinema, aes(x = num_film_serie, y = proportion, fill = sortie_cinema)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(proportion * 100, 1), " %")),
    position = position_stack(vjust = 0.5),  # centre sur chaque segment
    size = 2.5,
    color = "white"
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  labs(
    title = "Le nombre de sortie au cinéma en fonction de la fréquence de visionnage de film en streaming à la maison",
    x = "Fréquence films/séries",
    y = "Proportion",
    fill = "Nombre Sorties cinéma"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# ------------------------------ #
# 2 Musique × Sorties concerts
# ------------------------------ #
cross_musique_concert <- dataAnalyseAvecPoids %>%
  group_by(num_musique, sortie_concert) %>%
  summarise(effectif = sum(poids), .groups = "drop") %>%
  group_by(num_musique) %>%
  mutate(proportion = effectif / sum(effectif)) %>%
  ungroup()

cross_musique_concert$num_musique <- factor(
  cross_musique_concert$num_musique,
  levels = c(
    "Jamais",
    "Rarement, tous les ans",
    "Occasionnellement, tous les mois",
    "Souvent, toutes les semaines",
    "Très souvent, tous les jours"
  )
)


cross_musique_concert$sortie_concert <- factor(
  cross_musique_concert$sortie_concert,
  levels = c(
    "Plus de 10",
    "8 à 10",
    "6 à 8",
    "3 à 5",
    "1 ou 2",
    "0"
  )
)




# contingence_musique <- cross_musique_concert %>%
#   select(num_musique, sortie_concert, effectif) %>%
#   pivot_wider(names_from = sortie_concert, values_from = effectif) %>%
#   column_to_rownames(var = "num_musique") %>%
#   as.matrix()
# 
# chisq.test(contingence_musique,correct=F)$expected
# chisq.test(contingence_musique,correct=F)



print(n = Inf,cross_musique_concert )


ggplot(cross_musique_concert, aes(x = num_musique, y = proportion, fill = sortie_concert)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(proportion * 100, 1), " %")),
    position = position_stack(vjust = 0.5),  # centre le texte sur chaque segment
    size = 3.2,
    color = "white"
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  labs(
    title = "Le nombre de sortie au concert selon la fréquence d'écoute de musique à la maison (proportions)",
    x = "Fréquence écoute musique",
    y = "Proportion",
    fill = "Sorties concerts"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------ #
# 3 source réseaux sociaux × source bouche à oreille
# ------------------------------ #
cross_reseaux_bouche_oreille <- dataAnalyseAvecPoids %>%
  group_by(source_reseaux, source_bouche_oreille) %>%
  summarise(effectif = sum(poids), .groups = "drop") %>%
  group_by(source_reseaux) %>%
  mutate(proportion = effectif / sum(effectif)) %>%
  ungroup()


cross_reseaux_bouche_oreille$source_bouche_oreille <- factor(
  cross_reseaux_bouche_oreille$source_bouche_oreille,
  levels = c(
    "Oui",
    "Non"
  )
)



print(n = Inf,cross_reseaux_bouche_oreille )



# contingence_reseaux <- cross_reseaux_bouche_oreille %>%
#   select(source_reseaux, source_bouche_oreille, effectif) %>%
#   pivot_wider(names_from = source_bouche_oreille, values_from = effectif) %>%
#   column_to_rownames(var = "source_reseaux") %>%
#   as.matrix()
# 
# contingence_reseaux

# chisq.test(contingence_reseaux)$expected
# chisq.test(contingence_reseaux)




ggplot(cross_reseaux_bouche_oreille, aes(x = source_reseaux, y = proportion, fill = source_bouche_oreille)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(proportion * 100, 1), " %")),
    position = position_stack(vjust = 0.5),  # centre le texte sur chaque segment
    size = 3.2,
    color = "white"
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  labs(
    title = "Réseaux sociaux vs bouche à oreille (proportions)",
    x = "Usage réseau sociaux comme source d'information",
    y = "Proportion",
    fill = "Usage bouche à oreille comme source d'information"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))































