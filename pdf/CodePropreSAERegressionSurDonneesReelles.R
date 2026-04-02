
rm(list = ls())

setwd("D:/devoir de Enzo/DOSSIER IUT Paris cité SD 1ère année/Régression linéaire SAE/Rendu Projet")
.libPaths("Package")

load("Data_Projet4.RData")



# ====================================================================================================
#  0. Chargement des packages
# ====================================================================================================

library(ggplot2)
library(dplyr)
library(robusTest)  # Pour cortest
library(tidyr)



# ====================================================================================================
# 1. Statistique descriptive
# ====================================================================================================

# ==============================================================
# 1.a Analyse Univariée
# ==============================================================




### ====
### Sur l'Age (Variable quantitative continue)
### ====

summary(Data$Age) # Min; Max; Mean; + les 4 quartiles
sd(Data$Age) # Ecart type

# Histogramme
hist(Data$Age,
     main = "Distribution de l’âge",
     col = "skyblue",
     xlab = "Âge",
     border = "white")

# Boxplot
boxplot(Data$Age,
        horizontal = FALSE,
        main = "Boxplot sur la distribution de l’âge (en années décimales)",
        col = "lightgray",
        xlab = "Âge")




### ====
### Sur la Taille (Variable quantitative continue)
### ====


summary(Data$Taille) # Min; Max; Mean; + les 4 quartiles
sd(Data$Taille) # Ecart type
# Histogramme
hist(Data$Taille,
     main = "Distribution de la taille",
     col = "lightgreen",
     xlab = "Taille (cm)",
     border = "white")

# Boxplot
boxplot(Data$Taille,
        horizontal = FALSE,
        main = "Boxplot sur la distribution de la taille (en cm)",
        col = "lightyellow",
        xlab = "Taille (cm)")



# ==============================================================
# 1.b Analyse bivariée
# ==============================================================


### ====
### 1. Taille sachant Sexe (quantitative vs catégorielle)
### ====


tapply(Data$Taille, Data$Sexe, summary)
tapply(Data$Taille, Data$Sexe, sd)

boxplot(Taille ~ Sexe, data = Data,
        col = c("lightblue", "lightpink"),
        main = "Taille selon le sexe",
        ylab = "Taille (cm)")





# Test de fisher : taille selon le sexe
anova_taille_sexe <- aov(Taille ~ Sexe, data = Data)

# Affichage du résultat
summary(anova_taille_sexe)




### ====
### 2 Taille sachant Pays (quantitative vs catégorielle)
### ====

# Statistiques descriptives par pays
tapply(Data$Taille, Data$Pays, summary)

# Écart-type par pays
tapply(Data$Taille, Data$Pays, sd)

# Taille
boxplot(Taille ~ Pays, data = Data,
        col = c("gold", "lightgray"),
        main = "Taille selon le pays",
        ylab = "Taille (cm)")



# Test de fisher: taille selon pays
anova_taille_pays <- aov(Taille ~ Pays, data = Data)

# Afficher les résultats
summary(anova_taille_pays)





### ====
### 3. taille sachant l'âge  (quantitative vs quantitative)
### ====




# Nuage de points
plot(Data$Age, Data$Taille,
     col = "grey",
     pch = 5,
     cex = 0.1,
     main = "Nuage de point de la taille en fonction de l’âge",
     xlab = "Âge",
     ylab = "Taille (cm)")

# Coefficient de corrélation
cor(Data$Age, Data$Taille)

# Test de corrélation de Pearson
cor.test(Data$Age, Data$Taille, method = "pearson")








# ====================================================================================================
# 2. MODELISATION STATISTIQUE
# ====================================================================================================




# ==============================================================
##  2.1 AJUSTEMENT POLYNOMIAL DES COURBES DE CROISSANCE 
# ==============================================================



### ====
### 1. Régression polynomiale degré 1 à 4
### ====

# Ajustement d'un modèle linéaire
modele_lineaire <- lm(Taille ~ Age, data = Data)

# Ajustement des modèles polynomiaux (degré 2 à 4)
modele_poly2 <- lm(Taille ~ Age + I(Age^2), data = Data)
modele_poly3 <- lm(Taille ~ Age + I(Age^2) + I(Age^3), data = Data)
modele_poly4 <- lm(Taille ~ Age + I(Age^2) + I(Age^3) + I(Age^4), data = Data)

# Comparaison des modèles avec AIC
valeurs_aic <- c(AIC(modele_lineaire), AIC(modele_poly2), AIC(modele_poly3), AIC(modele_poly4))
names(valeurs_aic) <- c("Linéaire", "Degré 2", "Degré 3", "Degré 4")

# Affichage des valeurs AIC
print(valeurs_aic)

# Affichage des résumés des modèles
cat("Modèle Linéaire:\n")
print(summary(modele_lineaire))

cat("\nModèle Polynomial degré 2:\n")
print(summary(modele_poly2))

cat("\nModèle Polynomial degré 3:\n")
print(summary(modele_poly3))

cat("\nModèle Polynomial degré 4:\n")
print(summary(modele_poly4))



# Séquence d'âges pour les prédictions
xseq <- seq(min(Data$Age), max(Data$Age), length.out = 300)

# Prédictions
pred1 <- predict(modele_lineaire, newdata = data.frame(Age = xseq))
pred2 <- predict(modele_poly2, newdata = data.frame(Age = xseq))
pred3 <- predict(modele_poly3, newdata = data.frame(Age = xseq))
pred4 <- predict(modele_poly4, newdata = data.frame(Age = xseq))


# Nuage de points
plot(Data$Age, Data$Taille,
     col = "grey",
     pch = 5,
     cex = 0.1,
     main = "Courbes de croissance - Ajustement polynomial",
     xlab = "Âge",
     ylab = "Taille (cm)")



# Courbes avec styles différents
lines(xseq, pred1, col = "red", lwd = 2, lty = 2)     # Degré 1 
lines(xseq, pred2, col = "purple", lwd = 2, lty = 2)    # Degré 2 
lines(xseq, pred4, col = "yellow", lwd = 2, lty = 2)  # Degré 4 
lines(xseq, pred3, col = "green4", lwd = 3, lty = 1)  # Degré 3 

# Légende
legend("bottomright",
       legend = c("Degré 1", "Degré 2", "Degré 3", "Degré 4"),
       col = c("red", "blue", "green4", "yellow"),
       lty = c(2, 2, 1, 2), lwd = 2, bty = "n")













### ====
###  analyse des résidus (MODELE DE DEGRE 1)
### ====

# Valeurs ajustées
yhat1 <- fitted(model1)
e1 <- resid(model1)
std_e1 <- sd(e1)
std_e1

# Histogramme et boîte à moustaches

residus <- data.frame(Age = Data$Age, Residus = e1)

ggplot(residus, aes(x = Residus)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  labs(title = "Histogramme des résidus - Modèle degré 1")



ggplot(residus, aes(x = Age, y = Residus)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 2 * std_e1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = -2 * std_e1, color = "red", linetype = "dashed") +
  labs(title = "Nuage des résidus selon l'âge - Modèle degré 1")

# Compter combien sont en dehors de [-2*std_e1, +2*std_e1]
nb_hors_zone1 <- sum(e1 < -2 * std_e1 | e1 > 2 * std_e1)

# Afficher le résultat
print(nb_hors_zone1)





### ====
###  analyse des résidus (MODELE DE DEGRE 2)
### ====

yhat2 <- fitted(modele_poly2)
e2 <- resid(modele_poly2)
std_e2 <- sd(e2)
std_e2
residus2 <- data.frame(Age = Data$Age, Residus = e2)

# Histogramme
ggplot(residus2, aes(x = Residus)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogramme des résidus - Modèle degré 2")



# Nuage des résidus
ggplot(residus2, aes(x = Age, y = Residus)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 2 * std_e2, color = "red", linetype = "dashed") +
  geom_hline(yintercept = -2 * std_e2, color = "red", linetype = "dashed") +
  labs(title = "Nuage des résidus selon l'âge - Modèle degré 2")

# Compter résidus hors de [-2*std, 2*std]
nb_hors_zone2 <- sum(e2 < -2 * std_e2 | e2 > 2 * std_e2)
print(paste("Nombre de résidus hors zone ±2*std - degré 2 :", nb_hors_zone2))



### ====
###  analyse des résidus (MODELE DE DEGRE 3)
### ====

yhat3 <- fitted(modele_poly3)
e3 <- resid(modele_poly3)
std_e3 <- sd(e3)

residus3 <- data.frame(Age = Data$Age, Residus = e3)

ggplot(residus3, aes(x = Residus)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogramme des résidus - Modèle degré 3")



ggplot(residus3, aes(x = Age, y = Residus)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 2 * std_e3, color = "red", linetype = "dashed") +
  geom_hline(yintercept = -2 * std_e3, color = "red", linetype = "dashed") +
  labs(title = "Nuage des résidus selon l'âge - Modèle degré 3")

nb_hors_zone3 <- sum(e3 < -2 * std_e3 | e3 > 2 * std_e3)
print(paste("Nombre de résidus hors zone ±2*std - degré 3 :", nb_hors_zone3))



### ====
###  analyse des résidus (MODELE DE DEGRE 4)
### ====

yhat4 <- fitted(modele_poly4)
e4 <- resid(modele_poly4)
std_e4 <- sd(e4)

residus4 <- data.frame(Age = Data$Age, Residus = e4)

ggplot(residus4, aes(x = Residus)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogramme des résidus - Modèle degré 4")


ggplot(residus4, aes(x = Age, y = Residus)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 2 * std_e4, color = "red", linetype = "dashed") +
  geom_hline(yintercept = -2 * std_e4, color = "red", linetype = "dashed") +
  labs(title = "Nuage des résidus selon l'âge - Modèle degré 4")

nb_hors_zone4 <- sum(e4 < -2 * std_e4 | e4 > 2 * std_e4)
print(paste("Nombre de résidus hors zone ±2*std - degré 4 :", nb_hors_zone4))











# ==============================================================
##  2.2 courbe de croissance médiane, premier et troisième quartiles.
# ==============================================================


### ====
### 1. Création des sous-ensembles de données par sexe et pays
### ====

Data1 <- Data %>%
  filter(Sexe == "Boys", Pays == "United Arab Emirates")

Data2 <- Data %>%
  filter(Sexe == "Girls", Pays == "United Arab Emirates")

Data3 <- Data %>%
  filter(Sexe == "Boys", Pays == "New Zealand")

Data4 <- Data %>%
  filter(Sexe == "Girls", Pays == "New Zealand")


### ====
### 2. Définition des fonctions pour Q1 et Q3
### ====

Q1 <- function(x) {
  quantile(x, 0.25)
}

Q3 <- function(x) {
  quantile(x, 0.75)
}


### ====
### 3. tracés des croissance médiane, premier et troisième quartile pour chaque groupe 
### ====



## ---- Groupe 1 : Boys - UAE ----
newAge1 <- cut(Data1$Age, breaks = 5:19)
taille_median1 <- tapply(Data1$Taille, newAge1, median)
taille_median1
taille1_Q1 <- tapply(Data1$Taille, newAge1, Q1)
taille1_Q1
taille1_Q3 <- tapply(Data1$Taille, newAge1, Q3)
taille1_Q3

plot(Data1$Age, Data1$Taille,
     col = "grey",
     main = "Courbes de croissance médiane, Q1 et Q3 - Boys (UAE)",
     xlab = "Âge", ylab = "Taille (cm)")
lines(5.5:18.5, taille_median1, col = 'red')
lines(5.5:18.5, taille1_Q1, col = 'blue')
lines(5.5:18.5, taille1_Q3, col = 'blue')
legend("topleft", legend = c("Médiane", "1er quartile (Q1)", "3e quartile (Q3)"),
       col = c("blue", "red", "red"), lty = c(1, 1, 1), lwd = 1)



## ---- Groupe 2 : Girls - UAE ----
newAge2 <- cut(Data2$Age, breaks = 5:19)
taille_median2 <- tapply(Data2$Taille, newAge2, median)
taille_median2
taille2_Q1 <- tapply(Data2$Taille, newAge2, Q1)
taille2_Q1
taille2_Q3 <- tapply(Data2$Taille, newAge2, Q3)
taille2_Q3

plot(Data2$Age, Data2$Taille,
     col = "grey",
     main = "Courbes de croissance médiane, Q1 et Q3 - Girls (UAE)",
     xlab = "Âge", ylab = "Taille (cm)")
lines(5.5:18.5, taille_median2, col = 'red')
lines(5.5:18.5, taille2_Q1, col = 'blue')
lines(5.5:18.5, taille2_Q3, col = 'blue')
legend("topleft", legend = c("Médiane", "1er quartile (Q1)", "3e quartile (Q3)"),
       col = c("blue", "red", "red"), lty = c(1, 1, 1), lwd = 1)



## ---- Groupe 3 : Boys - New Zealand ----
newAge3 <- cut(Data3$Age, breaks = 5:19)
taille_median3 <- tapply(Data3$Taille, newAge3, median)
taille_median3
taille3_Q1 <- tapply(Data3$Taille, newAge3, Q1)
taille3_Q1
taille3_Q3 <- tapply(Data3$Taille, newAge3, Q3)
taille3_Q3

plot(Data3$Age, Data3$Taille,
     col = "grey",
     main = "Courbes de croissance médiane, Q1 et Q3 - Boys (New Zealand)",
     xlab = "Âge", ylab = "Taille (cm)")
lines(5.5:18.5, taille_median3, col = 'red')
lines(5.5:18.5, taille3_Q1, col = 'blue')
lines(5.5:18.5, taille3_Q3, col = 'blue')
legend("topleft", legend = c("Médiane", "1er quartile (Q1)", "3e quartile (Q3)"),
       col = c("blue", "red", "red"), lty = c(1, 1, 1), lwd = 1)



## ---- Groupe 4 : Girls - New Zealand ----
newAge4 <- cut(Data4$Age, breaks = 5:19)
taille_median4 <- tapply(Data4$Taille, newAge4, median)
taille_median4
taille4_Q1 <- tapply(Data4$Taille, newAge4, Q1)
taille4_Q1
taille4_Q3 <- tapply(Data4$Taille, newAge4, Q3)
taille4_Q3

plot(Data4$Age, Data4$Taille,
     col = "grey",
     main = "Courbes de croissance médiane, Q1 et Q3 - Girls (New Zealand)",
     xlab = "Âge", ylab = "Taille (cm)")
lines(5.5:18.5, taille_median4, col = 'red')
lines(5.5:18.5, taille4_Q1, col = 'blue')
lines(5.5:18.5, taille4_Q3, col = 'blue')
legend("topleft", legend = c("Médiane", "1er quartile (Q1)", "3e quartile (Q3)"),
       col = c("blue", "red", "red"), lty = c(1, 1, 1), lwd = 1)




### ====
### 3. Comparaison des courbes de médiane entre les 4 groupes
### ====


age_midpoints <- seq(5.5, 18.5, by = 1)


plot(age_midpoints, taille_median1, type = "l", col = "black",
     ylim = range(c(taille_median1, taille_median2, taille_median3, taille_median4)),
     xlab = "Tranche d'âge (milieu)",
     ylab = "Taille (cm)",
     main = "Comparaison des courbes de médiane")

lines(age_midpoints,taille_median2, col = "red")
lines(age_midpoints,taille_median3, col = "blue")
lines(age_midpoints,taille_median4, col = "green")

legend("topleft",
       legend = c("Boys - UAE", "Girls - UAE", "Boys - NZ", "Girls - NZ"),
       col = c("black", "red", "blue", "green"),
       lty = 1)

