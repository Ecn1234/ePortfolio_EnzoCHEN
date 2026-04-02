# Compte rendu : SAE Estimation par échantillonnage
# CHEN Enzo, DAI Michael, MA Hugo
# BUT 1 SD — Groupe 11

rm(list = ls())

# ENVIRONNEMENT ----
# setwd("C:/Users/michael.dai/OneDrive - Université de Paris/SAE/Estimation par échantillonnage Avr25/Travail SAE")
# setwd("C:/Users/micha/OneDrive - Université de Paris/SAE/Estimation par échantillonnage Avr25/Travail SAE")

require(tidyverse)
require(matrixStats)

# IMPORT DES DONNÉES ----
## Import des fichiers ----
Vmag30 = read.table("vmag30_1.txt", header = TRUE)
View(Vmag30)

Vmag100 = read.table("vmag100_1.txt", header = TRUE)
View(Vmag100)

Stars = read.csv("Star39552_balanced.csv")
View(Stars)

mutheo = mean(Stars$Vmag)
mutheo

# ANALYSE DES DONNÉES ----
## 1. Échantillon de taille 30 ----

### 1.1 Statistiques descriptives ----
ggplot(data = Vmag30) +
  aes(y = Vmag) +
  labs(title = "Boîte à moustaches de Vmag, échantillon taille 30",
       y = "Vmag = magnitude apparente de l’étoile") +
  geom_boxplot()

boxplot(Vmag30$Vmag,
        main = "Boîte à moustaches de Vmag, échantillon taille 30",
        ylab = "Vmag = magnitude apparente de l’étoile")

ggplot(data = Vmag30) +
  aes(x = Vmag) +
  labs(title = "Histogramme de Vmag, échantillon taille 30",
       x = "Vmag = magnitude apparente de l’étoile",
       y = "Effectif") +
  geom_histogram(binwidth = 0.5) +
  scale_x_continuous(breaks = seq(3, 12, by = 1)) +
  theme_minimal()

summary(Vmag30)
sd(Vmag30$Vmag)
var(Vmag30$Vmag)

### 1.2 Intervalle de confiance ----

#### 1.2.1 Niveau de risque 95 % ----
Xbar_30 = mean(Vmag30$Vmag)
sigma_30 = sd(Vmag30$Vmag)
alpha = 1 - 0.95
n = 30

qt((1 - (alpha/2)), n - 1)
Ecart = qt((1 - (alpha/2)), n - 1) * sigma_30 / sqrt(n)

a = Xbar_30 - Ecart
b = Xbar_30 + Ecart

print(paste("Xbar =", Xbar_30, "IC", 100*(1-alpha), "% [", a, "-", b, "]"))

#### 1.2.2 Niveau de risque 99 % ----
alpha = 1 - 0.99

qt((1 - (alpha/2)), n - 1)
Ecart = qt((1 - (alpha/2)), n - 1) * sigma_30 / sqrt(n)

a = Xbar_30 - Ecart
b = Xbar_30 + Ecart

print(paste("Xbar =", Xbar_30, "IC", 100*(1-alpha), "% [", a, "-", b, "]"))

### 1.3 Test de comparaison ----

#### 1.3.1 Niveau de risque 95 % ----

##### 1.3.1.1 Statistique de test ----
mu0 = 8
alpha = 0.05

t = sqrt(n) * (Xbar_30 - mu0) / sigma_30
t

##### 1.3.1.2 Quantile du test ----
qt((1 - (alpha/2)), n - 1)

##### 1.3.1.3 p-valeur du test ----
pval = 2 * (pt(abs(t), n - 1, lower.tail = FALSE))
pval

#### 1.3.2 Niveau de risque 99 % ----

##### 1.3.2.1 Statistique de test ----
alpha = 0.01

t = sqrt(n) * (Xbar_30 - mu0) / sigma_30
t

##### 1.3.2.2 Quantile du test ----
qt((1 - (alpha/2)), n - 1)

##### 1.3.2.3 p-valeur du test ----
pval = 2 * (pt(abs(t), n - 1, lower.tail = FALSE))
pval

### 1.4 Test avec les intervalles de confiance ----

#### 1.4.1 Niveau de risque 95 % ----
alpha = 1 - 0.95

Ecart = qt((1 - (alpha/2)), n - 1) * sigma_30 / sqrt(n)
a = Xbar_30 - Ecart
b = Xbar_30 + Ecart

print(paste("Mu0 =", mu0, "IC", 100*(1-alpha), "% [", a, "-", b, "]"))

#### 1.4.2 Niveau de risque 99 % ----
alpha = 1 - 0.99

Ecart = qt((1 - (alpha/2)), n - 1) * sigma_30 / sqrt(n)
a = Xbar_30 - Ecart
b = Xbar_30 + Ecart

print(paste("Mu0 =", mu0, "IC", 100*(1-alpha), "% [", a, "-", b, "]"))

### 1.5 Test avec l’instruction R t.test ----

#### 1.5.1 Niveau de risque 95 % ----
alpha = 1 - 0.95
t.test(Vmag30, mu = mu0, conf.level = 1 - alpha)

#### 1.5.2 Niveau de risque 99 % ----
alpha = 1 - 0.99
t.test(Vmag30, mu = mu0, conf.level = 1 - alpha)

### 1.6 Densité de la loi de la statistique de test sous H0 ----

#### 1.6.1 Niveau de risque 95 % ----
alpha = 1 - 0.95

curve(dt(x, n - 1), col = "black", from = -3.5, to = 3.5, xlab = "Graphique alpha = 5 %")
abline(v = t, lwd = 2, col = "blue")  # Statistique de test observée
abline(v = qt((1 - (alpha/2)), n - 1), lwd = 2, col = "red")  # Quantile 97,5 %
abline(v = qt((alpha/2), n - 1), lwd = 2, col = "red")        # Quantile 2,5 %

#### 1.6.2 Niveau de risque 99 % ----
alpha = 1 - 0.99

curve(dt(x, n - 1), col = "black", from = -3.5, to = 3.5, xlab = "Graphique alpha = 1 %")
abline(v = t, lwd = 2, col = "blue")  # Statistique de test observée
abline(v = qt((1 - (alpha/2)), n - 1), lwd = 2, col = "red")  # Quantile 99,5 %
abline(v = qt((alpha/2), n - 1), lwd = 2, col = "red")        # Quantile 0,5 %

##################################################################

## 2. Échantillon de taille 100 ----

### 2.1 Statistiques descriptives ----
ggplot(data = Vmag100) +
  aes(y = Vmag) +
  labs(title = "Boîte à moustaches de Vmag, échantillon taille 100",
       y = "Vmag = magnitude apparente de l’étoile") +
  geom_boxplot()

boxplot(Vmag100$Vmag,
        main = "Boîte à moustaches de Vmag, échantillon taille 100",
        ylab = "Vmag = magnitude apparente de l’étoile")

ggplot(data = Vmag100) +
  aes(x = Vmag) +
  labs(title = "Histogramme de Vmag, échantillon taille 100",
       x = "Vmag = magnitude apparente de l’étoile",
       y = "Effectif") +
  geom_histogram(binwidth = 0.5) +
  scale_x_continuous(breaks = seq(3, 12, by = 1)) +
  theme_minimal()

summary(Vmag100)
sd(Vmag100$Vmag)
var(Vmag100$Vmag)

### 2.2 Intervalle de confiance ----

#### 2.2.1 Niveau de risque 95 % ----
Xbar_100 = mean(Vmag100$Vmag)
sigma_100 = sd(Vmag100$Vmag)
alpha = 1 - 0.95
n = 100

qt((1 - (alpha/2)), n - 1)
Ecart = qt((1 - (alpha/2)), n - 1) * sigma_100 / sqrt(n)

a = Xbar_100 - Ecart
b = Xbar_100 + Ecart

print(paste("Xbar =", Xbar_100, "IC", 100*(1-alpha), "% [", a, "-", b, "]"))

#### 2.2.2 Niveau de risque 99 % ----
alpha = 1 - 0.99

qt((1 - (alpha/2)), n - 1)
Ecart = qt((1 - (alpha/2)), n - 1) * sigma_100 / sqrt(n)

a = Xbar_100 - Ecart
b = Xbar_100 + Ecart

print(paste("Xbar =", Xbar_100, "IC", 100*(1-alpha), "% [", a, "-", b, "]"))

### 2.3 Test de comparaison ----

#### 2.3.1 Niveau de risque 95 % ----

##### 2.3.1.1 Statistique de test ----
mu0 = 8
alpha = 0.05

t = sqrt(n) * (Xbar_100 - mu0) / sigma_100
t

##### 2.3.1.2 Quantile du test ----
qt((1 - (alpha/2)), n - 1)

##### 2.3.1.3 p-valeur du test ----
pval = 2 * (pt(abs(t), n - 1, lower.tail = FALSE))
pval

#### 2.3.2 Niveau de risque 99 % ----

##### 2.3.2.1 Statistique de test ----
alpha = 0.01

t = sqrt(n) * (Xbar_100 - mu0) / sigma_100
t

##### 2.3.2.2 Quantile du test ----
qt((1 - (alpha/2)), n - 1)

##### 2.3.2.3 p-valeur du test ----
pval = 2 * (pt(abs(t), n - 1, lower.tail = FALSE))
pval

### 2.4 Test avec les intervalles de confiance ----

#### 2.4.1 Niveau de risque 95 % ----
alpha = 1 - 0.95

Ecart = qt((1 - (alpha/2)), n - 1) * sigma_100 / sqrt(n)
a = Xbar_100 - Ecart
b = Xbar_100 + Ecart

print(paste("Mu0 =", mu0, "IC", 100*(1-alpha), "% [", a, "-", b, "]"))

#### 2.4.2 Niveau de risque 99 % ----
alpha = 1 - 0.99

Ecart = qt((1 - (alpha/2)), n - 1) * sigma_100 / sqrt(n)
a = Xbar_100 - Ecart
b = Xbar_100 + Ecart

print(paste("Mu0 =", mu0, "IC", 100*(1-alpha), "% [", a, "-", b, "]"))

### 2.5 Test avec l’instruction R t.test ----

#### 2.5.1 Niveau de risque 95 % ----
alpha = 1 - 0.95
t.test(Vmag100, mu = mu0, conf.level = 1 - alpha)

#### 2.5.2 Niveau de risque 99 % ----
alpha = 1 - 0.99
t.test(Vmag100, mu = mu0, conf.level = 1 - alpha)

### 2.6 Densité de la loi de la statistique de test sous H0 ----

#### 2.6.1 Niveau de risque 95 % ----
alpha = 1 - 0.95

curve(dt(x, n - 1), col = "black", from = -3.5, to = 3.5, xlab = "Graphique alpha = 5 %")
abline(v = t, lwd = 2, col = "blue")  # Statistique de test observée
abline(v = qt((1 - (alpha/2)), n - 1), lwd = 2, col = "red")  # Quantile 97,5 %
abline(v = qt((alpha/2), n - 1), lwd = 2, col = "red")        # Quantile 2,5 %

#### 2.6.2 Niveau de risque 99 % ----
alpha = 1 - 0.99

curve(dt(x, n - 1), col = "black", from = -3.5, to = 3.5, xlab = "Graphique alpha = 1 %")
abline(v = t, lwd = 2, col = "blue")  # Statistique de test observée
abline(v = qt((1 - (alpha/2)), n - 1), lwd = 2, col = "red")  # Quantile 99,5 %
abline(v = qt((alpha/2), n - 1), lwd = 2, col = "red")        # Quantile 0,5 %
