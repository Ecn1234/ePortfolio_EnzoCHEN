rm(list = ls())

# ===== #
# Environnement de travail ----
# ===== #

setwd("D:/devoir de Enzo/IUT 2eme annee/TP_testStatCompteRendu")
.libPaths("Packages")
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("questionr")
# install.packages("skimr")
# install.packages("GGally")

library(tidyverse)
library(GGally)
library(skimr)
library(corrplot)
library(questionr)


# Charger les données
don=read.csv("Donnees/CardioGoodFitness.csv",header=TRUE,sep=",")

# Structure des données
str(don)

# Nombre de NA par variable
colSums(is.na(don))
summary(don)


# ====== #
#question 1. ----
# =======#

#transformer les var char en factor
#creation de la variable Niv
don<-don %>% mutate(
  Fitness = factor(Fitness),
  Product = factor(Product),
  Gender = factor(Gender),
  MaritalStatus = factor(MaritalStatus),
  Niv = cut(Education, breaks = c(11, 14, 16, 21), labels = c("12-14", "14-16", ">16")),
  Usage_cat= case_when(
    Usage <= 3 ~ "Faible",
    Usage >3 & Usage <=5 ~ "Modere",
    Usage >5 ~ "Eleve",
    .default="Autre"
  )%>%factor(levels=c("Faible","Modere","Eleve"))
)




don <- don %>% 
  mutate(Gender = ifelse(Gender == "Male", "Homme", "Femme"),
         MaritalStatus = ifelse(MaritalStatus == "Single", "Célibataire","En couple"))



summary(don)




#selection de sous ensemble de données
#les clients ayant achete un modele particulier
donTM195<-don %>%
  filter(Product=="TM195")
donTM498<-don %>%
  filter(Product=="TM498")
donTM798<-don %>%
  filter(Product=="TM798")

#les clients hommes et femmes
donF<-don %>%
  filter(Gender=="Femme")
donM<-don %>%
  filter(Gender=="Homme")

#analyse descriptive de la base
summary(don)
#analyse des variables quali Product, Gender, MaritalStatus, Fitness
freq(don$Gender,total=TRUE)
freq(don$Product,sort="inc",total=TRUE)
freq(don$MaritalStatus,sort="inc",total=TRUE)
freq(don$Fitness,total=TRUE,valid=FALSE)

#analyse des variables quanti
num_data <- don %>%
  select(where(is.numeric))
#avec package skim
skim_without_charts(num_data)





##analyse bivariee

#question 2.----
#question 2.a ----
#Lien revenu et genre : 1 var quanti et 1 var quali dont les modalites definissent 2 groupes H et F
#statdes
don%>%group_by(Gender)%>%
  summarize(
    nb = n(),
    moy = mean(Income,na.rm=T),
    sd = sd(Income,na.rm=T),
    q1=quantile(Income,prob=0.25),
    Med=quantile(Income,prob=0.5),
    q3=quantile(Income,prob=0.75)
  )
#de maniere equivalente

don%>%
  group_by(Gender)%>%
  skim_without_charts(Income)
#de maniere equivalente avec Rbase
by(don$Income,don$Gender,summary)

#boxplot par genre à commenter
ggplot(data=don,aes(Gender,Income))+
  geom_boxplot()+
  labs(title = "La répartition du revenu en fonction du genre", x = "Genre", y = "Revenu annuel en dollar")+
  geom_hline(yintercept=mean(donF$Income),col="red")+
  geom_hline(yintercept=mean(donM$Income),col="blue")

#boxplot avec Rbase
boxplot(don$Income~don$Gender, main="Revenu en fonction du genre", xlab="Genre",ylab="Revenu annuel en dollar", names=c("Femme","Homme"),col=c("red","blue"))
abline(h=mean(donF$Income),col="red")
abline(h=mean(donM$Income),col="blue")
legend(legend=c("MoyF", "MoyH"),col=c("red","blue"),pch=15, "right")

#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands
t.test(Income~Gender,data=don)
#ou bien
t.test(donF$Income,donM$Income)
#p-value = 0.004021 on rejette H0 le revenu depend significativement du genre

#question 2.b ----
#Lien produit et genre : 2 var quali
#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Gender,don$Product)
#dist conditionnelle du produit sachant genre
lprop(table(don$Gender,don$Product))
#dist conditionnelle du genre sachant produit
cprop(table(don$Gender,don$Product))
#graphique
#ggplot
ggplot(don) +
  aes(x = Gender, fill = Product)+
  geom_bar(
    position = "fill"
  )+
  labs(title="La répartition du modèle du tapis acheté selon le genre",
       x="Genre",y="fréquence",fill="Modèle du tapis acheté")
#Rbase
barplot(prop.table(table(don$Product,don$Gender),margin=2),xlab="Genre",col=c("blue","green","red"), main="Produit acheté \n selon le genre")
legend(legend=c("TM195", "TM498","TM798"),col=c("blue","green","red"),pch=15, "right")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X genre, Y produit)
#effectifs theoriques
chisq.test(table(don$Gender,don$Product),correct=F)$expected
#tous plus grands que 5
chisq.test(table(don$Gender,don$Product),correct=F)
#p-value = 0.001562 on rejette H0 : le produit achete depend significatovement du genre

#question 2.c ----
#test comparaison proportion en couple Homme et femme
table(don$Gender,don$MaritalStatus)
lprop(table(don$Gender,don$MaritalStatus))
# parmi les femmes il y 60.5% de clients en couple contre 58.7% chez les hommes
#H0 pF=pH contre $H1:pF>pH
#conditions a verifier pour TCL n1pn, n1(1-pn),n2pn,n2(1-pn) >5, et n1,n2 grands
n1<-length(donF$MaritalStatus)
n1
n2<-length(donM$MaritalStatus)
n2
p1<-lprop(table(don$Gender,don$MaritalStatus))[1,1]/100
p1
p2<-lprop(table(don$Gender,don$MaritalStatus))[2,1]/100
p2
pn<-(n1*p1+n2*p2)/(n1+n2)
pn
n1*pn
n1*(1-pn)
n2*pn
n2*(1-pn)
prop.test(table(don$Gender,don$MaritalStatus),correct=F,alternative="greater")
#p-value = 0.4002 la proportion de clients en couple qui sont des F n'est pas significativement superieure à celle des H

#on retrouve les conditions via les eff theo du test du chi2
chisq.test(table(don$Gender,don$MaritalStatus),correct=F)$expected

#question 2.d ----

#dist conditionnelle du produit sachant genre
lprop(table(don$Gender,don$Usage_cat))

#lien entre Usage_cat et Genre
ggplot(don) +
  aes(x = Gender, fill = Usage_cat)+
  geom_bar(
    position = "fill"
  )+
  labs(title="Répartition de l'usage selon le genre",
       x="Genre",y="fréquence",fill="Usage_cat")
#lien entre usage_cat et genre significatif?
chisq.test(table(don$Usage_cat,don$Gender),correct=F)$expected
#1 eff theo <5 l'approximation du chi2 n'est pas bonne
chisq.test(table(don$Usage_cat,don$Gender),correct=F)
#on utilise le test exact de fisher
fisher.test(table(don$Usage_cat,don$Gender))

#question 2.e ----
#Lien genre et var quantitative
#stat des par genre de toutes les var quanti
don%>% 
  group_by(Gender)%>%
  skim_without_charts(where(is.numeric))

#Age
#statdes
#graphique
#test statistique 

#statdes
don%>%group_by(Gender)%>%
  summarize(
    nb = n(),
    moy = mean(Age,na.rm=T),
    sd = sd(Age,na.rm=T),
    q1=quantile(Age,prob=0.25),
    Med=quantile(Age,prob=0.5),
    q3=quantile(Age,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=don,aes(Gender,Age))+
  geom_boxplot()+
  labs(title = "La répartition de l'âge en fonction du genre", x = "Genre", y = "Age")+
  geom_hline(yintercept=mean(donF$Age),col="red")+
  geom_hline(yintercept=mean(donM$Age),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donF$Age,donM$Age)
#p-value = 0.7071 on ne rejette pas H0 l'age ne depend pas significativement du genre


#Miles
#statdes
#graphique
#test statistique 

#statdes
don%>%group_by(Gender)%>%
  summarize(
    nb = n(),
    moy = mean(Miles,na.rm=T),
    sd = sd(Miles,na.rm=T),
    q1=quantile(Miles,prob=0.25),
    Med=quantile(Miles,prob=0.5),
    q3=quantile(Miles,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=don,aes(Gender,Miles))+
  geom_boxplot()+
  labs(title = "La répartition de la distance (moyenne) parcourue attendue par semaine en fonction du genre", x = "Genre", y = "Distance (Miles)")+
  geom_hline(yintercept=mean(donF$Miles),col="red")+
  geom_hline(yintercept=mean(donM$Miles),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donF$Miles,donM$Miles)
#p-value = 0.002467 on  rejette  H0 la distance depend significativement du genre


#Education
#statdes
#graphique
#test statistique 


#statdes
don%>%group_by(Gender)%>%
  summarize(
    nb = n(),
    moy = mean(Education,na.rm=T),
    sd = sd(Education,na.rm=T),
    q1=quantile(Education,prob=0.25),
    Med=quantile(Education,prob=0.5),
    q3=quantile(Education,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=don,aes(Gender,Education))+
  geom_boxplot()+
  labs(title = "La répartition du nombre d'années d'études en fonction du genre", x = "Genre", y = "Nombre d'années d'études")+
  geom_hline(yintercept=mean(donF$Education),col="red")+
  geom_hline(yintercept=mean(donM$Education),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donF$Education,donM$Education)
#p-value = 0.1965 on ne rejette pas H0 l'Education ne depend pas significativement du genre




#Usage
#statdes
#graphique
#test statistique 


#statdes
don%>%group_by(Gender)%>%
  summarize(
    nb = n(),
    moy = mean(Usage,na.rm=T),
    sd = sd(Usage,na.rm=T),
    q1=quantile(Usage,prob=0.25),
    Med=quantile(Usage,prob=0.5),
    q3=quantile(Usage,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=don,aes(Gender,Usage))+
  geom_boxplot()+
  labs(title = "La répartition du nombre moyen d’utilisation prévu par semaine selon le genre ", x = "Genre", y = "Nombre moyen d'utilisation prévu par semaine")+
  geom_hline(yintercept=mean(donF$Usage),col="red")+
  geom_hline(yintercept=mean(donM$Usage),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donF$Usage,donM$Usage)
#p-value = 0.003484 on  rejette H0 l'Usage  depend  significativement du genre








for (v in c("Age","Usage","Income","Miles","Education")){
  cat("\nVariable:", v, "\n")
  summary_by <- don %>% group_by(Gender) %>% summarise(n=n(), mean=mean(.data[[v]], na.rm=TRUE), sd=sd(.data[[v]], na.rm=TRUE))
  print(summary_by)
  # t-test (Welch) :
  ttest <- t.test(don[[v]] ~ don$Gender)
  print(ttest[c("p.value")])
}

#lien genre et var qualitative Usage_cat, Fitness, Niv, MaritalStatus

#vous pouvez contruire de manirere automatisee pour chaque variable (avec une boucle sur les variables) une sortie 
#presentant le tableau de contingence ou dist conditionnelle sachant le genre, les effectifs theoriques
# et la pvaleur du test 


# Gender vs Niv

#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Gender,don$Niv)
#dist conditionnelle du Niv sachant produit
lprop(table(don$Gender,don$Niv))
#dist conditionnelle du Gender sachant Niv
cprop(table(don$Gender,don$Niv))
#graphique
#ggplot

don$Niv <- factor(don$Niv, levels = rev(levels(don$Niv)))

ggplot(don) +
  aes(x = Gender, fill = Niv)+
  geom_bar(
    position = "fill"
  )+
  labs(title="La répartition du niveau d'étude selon le genre",
       x="Genre",y="fréquence",fill="Niveau d'étude")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X Niv, Y produit)
#effectifs theoriques
chisq.test(table(don$Gender,don$Niv),correct=F)$expected
#tous plus grands que 5
chisq.test(table(don$Gender,don$Niv),correct=F)





# Gender vs Fitness

#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Gender,don$Fitness)
#dist conditionnelle du Fitness sachant produit
lprop(table(don$Gender,don$Fitness))
#dist conditionnelle du produit sachant Fitness
cprop(table(don$Gender,don$Fitness))
#graphique
#ggplot
don$Fitness <- factor(don$Fitness, levels = rev(levels(don$Fitness)))

ggplot(don) +
  aes(x = Gender, fill = Fitness)+
  geom_bar(
    position = "fill"
  )+
  labs(title="La répartition de la condition physique auto-évaluée selon le genre",
       x="Genre",y="fréquence",fill="Condition physique \nauto-évaluée")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X Fitness, Y produit)
#effectifs theoriques
chisq.test(table(don$Gender,don$Fitness),correct=F)$expected
#Ne sont pas tous plus grands que 5
fisher.test(table(don$Gender,don$Fitness),simulate.p.value = TRUE)

#p-value = 0.009495 on  rejette  H0 : le Fitness  depend  significatovement du genre


# Gender vs Usage_cat

#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Gender,don$Usage_cat)
#dist conditionnelle du Usage_cat sachant produit
lprop(table(don$Gender,don$Usage_cat))
#dist conditionnelle du produit sachant Usage_cat
cprop(table(don$Gender,don$Usage_cat))
#graphique
#ggplot

don$Usage_cat <- factor(don$Usage_cat, levels = rev(levels(don$Usage_cat)))

ggplot(don) +
  aes(x = Gender, fill = Usage_cat)+
  geom_bar(
    position = "fill"
  )+
  labs(title="La répartition de la fréquence d'usage prévue par semaine selon le genre",
       x="Genre",y="fréquence",fill="Fréquence d'usage")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X Usage_cat, Y produit)
#effectifs theoriques
chisq.test(table(don$Gender,don$Usage_cat),correct=F)$expected
#Ne sont pas tous plus grands que 5
fisher.test(table(don$Gender,don$Usage_cat))

#p-value = 0.0004998 on  rejette  H0 : l'usage cat  depend  significatovement du produit


# Gender vs MaritalStatus

#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Gender,don$MaritalStatus)
#dist conditionnelle du MaritalStatus sachant produit
lprop(table(don$Gender,don$MaritalStatus))
#dist conditionnelle du produit sachant MaritalStatus
cprop(table(don$Gender,don$MaritalStatus))
#graphique
#ggplot
ggplot(don) +
  aes(x = Gender, fill = MaritalStatus)+
  geom_bar(
    position = "fill"
  )+
  labs(title="La répartition du statut matrimonial selon le genre",
       x="Genre",y="fréquence",fill="Statut matrimonial")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X MaritalStatus, Y produit)
#effectifs theoriques
chisq.test(table(don$Gender,don$MaritalStatus),correct=F)$expected
#tous plus grands que 5
chisq.test(table(don$Gender,don$MaritalStatus),correct=F)
#p-value = 0.9605 on ne rejette pas H0 : le statut marital ne depend pas significatovement du produit




#question 3. ----
#lien entre produit et var quali  (Niv, Fitness, Usage_cat, MaritalStatus, Gender (deja fait))


# Product vs Niv

#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Product,don$Niv)
#dist conditionnelle du Niv sachant produit
lprop(table(don$Product,don$Niv))
#dist conditionnelle du produit sachant Niv
cprop(table(don$Product,don$Niv))
#graphique

don$Niv <- factor(don$Niv, levels = rev(levels(don$Niv)))


#ggplot
ggplot(don) +
  aes(x = Product, fill = Niv)+
  geom_bar(
    position = "fill"
  )+
  labs(title="La répartition du nombre d'années d'études selon le modèle du tapis acheté",
       x="Modèle du tapis acheté",y="fréquence",fill="Nombre d'années d'études")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X Niv, Y produit)
#effectifs theoriques
chisq.test(table(don$Product,don$Niv),correct=F)$expected
#tous plus grands que 5
chisq.test(table(don$Product,don$Niv),correct=F)
#p-value = 8.376e-16 on rejette H0 : le Niv  depend significatovement du produit




# Product vs Fitness

#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Product,don$Fitness)
#dist conditionnelle du Fitness sachant produit
lprop(table(don$Product,don$Fitness))
#dist conditionnelle du produit sachant Fitness
cprop(table(don$Product,don$Fitness))
#graphique
#ggplot

don$Fitness <- factor(don$Fitness, levels = rev(levels(don$Fitness)))


ggplot(don) +
  aes(x = Product, fill = Fitness)+
  geom_bar(
    position = "fill"
  )+
  labs(title="La répartition de la condition physique auto-évaluée selon le modèle acheté",
       x="Modèle du tapis acheté",y="fréquence",fill="condition physique \nauto-évaluée")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X Fitness, Y produit)
#effectifs theoriques
chisq.test(table(don$Product,don$Fitness),correct=F)$expected
#Ne sont pas tous plus grands que 5
fisher.test(table(don$Product,don$Fitness),simulate.p.value = TRUE)

#p-value = 0.0004998 on  rejette  H0 : le Fitness  depend  significatovement du produit


# Product vs Usage_cat

#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Product,don$Usage_cat)
#dist conditionnelle du Usage_cat sachant produit
lprop(table(don$Product,don$Usage_cat))
#dist conditionnelle du produit sachant Usage_cat
cprop(table(don$Product,don$Usage_cat))
#graphique
#ggplot

don$Usage_cat <- factor(don$Usage_cat, levels = rev(levels(don$Usage_cat)))

ggplot(don) +
  aes(x = Product, fill = Usage_cat)+
  geom_bar(
    position = "fill"
  )+
  labs(title="La répartition de la fréquence d'usage prévue par semaine selon le modèle acheté",
       x="Modèle du tapis acheté",y="Fréquence",fill="Fréquence d'usage")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X Usage_cat, Y produit)
#effectifs theoriques
chisq.test(table(don$Product,don$Usage_cat),correct=F)$expected
#Ne sont pas tous plus grands que 5
fisher.test(table(don$Product,don$Usage_cat))

#p-value = 0.0004998 on  rejette  H0 : l'usage cat  depend  significatovement du produit


# Product vs MaritalStatus

#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Product,don$MaritalStatus)
#dist conditionnelle du MaritalStatus sachant produit
lprop(table(don$Product,don$MaritalStatus))
#dist conditionnelle du produit sachant MaritalStatus
cprop(table(don$Product,don$MaritalStatus))
#graphique
#ggplot
ggplot(don) +
  aes(x = Product, fill = MaritalStatus)+
  geom_bar(
    position = "fill"
  )+
  labs(title="Répartition du statut matrimonial selon le modèle acheté",
       x="Modèle acheté",y="Fréquence",fill="Statut matrimonial")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X MaritalStatus, Y produit)
#effectifs theoriques
chisq.test(table(don$Product,don$MaritalStatus),correct=F)$expected
#tous plus grands que 5
chisq.test(table(don$Product,don$MaritalStatus),correct=F)
#p-value = 0.9605 on ne rejette pas H0 : le statut marital ne depend pas significatovement du produit




# Product vs Gender

#statdes : tableaux de contingence et distributions conditionnelles en freq 
#eff observe nij
table(don$Product,don$Gender)
#dist conditionnelle du genre sachant produit
lprop(table(don$Product,don$Gender))
#dist conditionnelle du produit sachant genre
cprop(table(don$Product,don$Gender))
#graphique
#ggplot
ggplot(don) +
  aes(x = Product, fill = Gender)+
  geom_bar(
    position = "fill"
  )+
  labs(title="Répartition des genres selon le produit",
       x="Produit",y="fréquence",fill="Genre")

#test du chi2 d'independance
#H0 X et Y independants, H1 X et Y liees (X genre, Y produit)
#effectifs theoriques
chisq.test(table(don$Product,don$Gender),correct=F)$expected
#tous plus grands que 5
chisq.test(table(don$Product,don$Gender),correct=F)
#p-value = 0.001562 on rejette H0 : le genre  depend significatovement du produit



#question 4 ----
#comparaison variance revenu selon statut matrimonial
#X revenu en couple, Y revenu celibataire
#cas gaussien? 
#H0:X suit Normale H1: X ne suit pas une loi normale
shapiro.test(don$Income[don$MaritalStatus=="Partnered"])
#p-value = 6.977e-08 on rejette H0 : on rejette la normalite des revenus en couple
#H0:Y suit Normale H1: Y ne suit pas une loi normale
shapiro.test(don$Income[don$MaritalStatus=="Single"])
#p-value = 2.879e-06 on rejette H0 : on rejette la normalite des revenus célibataires
boxplot (don$Income~don$MaritalStatus)
# pas etonnant de rejeter la normalite etant donne la presence de nombreux outliers dans les bocplot, distribution non symetrique
var.test(don$Income ~ don$MaritalStatus)

#question 5. ----
#lien entre var quanti

#question 5.a ----
#Analyse du lien entre Revenu et Age : lien entre 2 var quanti
ggplot(don)+
  aes(x=Age,y=Income)+
  geom_point()+
  labs(title = "Revenu des clients en fonction de l'âge", x = "Âge", y = "Revenu annuel ($)")
#Rbase
plot(don$Age,don$Income,main = "Revenu en fonction de l'âge", xlab = "Âge", ylab = "Revenu")
#coefficient de correlation de pearson et spearman
cor(don$Age,don$Income)
cor(don$Age,don$Income,method="spearman")

#question 5.b ----
#test de correlation Age et Income : pearson
#H0: coeff correlation pearson (X,Y)=0 H1 : coeff correlation pearson (X,Y) diff 0
# n grand ok pour approximation par N(0,1)
cor(don$Age,don$Income)
cor.test(don$Age,don$Income)
#p-value < 1.708e-13 on rejette H0, le revenu dépend significativement de l'Age

#H0: coeff correlation spearman (X,Y)=0 H1 : coeff correlation spearman (X,Y) diff 0
# n grand ok pour approximation par N(0,1)
#test de correlation Age et Income : spearman
cor(don$Age,don$Income,method="spearman")
cor.test(don$Age, don$Income, method="spearman")
#p-value < 2.2e-16 on rejette H0, le revenu dépend significativement de l'Age


#question 5.c ----
#correlation par produit
ggplot(data=don)+
  aes(x=Age,y=Income,color=Product)+
  geom_point()+
  labs(title = "La répartition du revenu annuel en dollar en fonction de l'âge par produit", x = "Âge", y = "Revenu annuel en dollar")
#+
#  facet_wrap(~Product) 
plot(don$Age,don$Income,col=don$Product,main = "Revenu en fonction de l'âge par produit",xlab = "Âge", ylab = "Revenu")
legend(legend=c("TM195", "TM498","TM798"),col=c("black","red","green"),pch=15, "right")
#correlation par produit
cor.test(donTM798$Age,donTM798$Income,method="spearman")
#ou de maniere equivalente
cor.test(don$Age[don$Product=="TM798"],don$Income[don$Product=="TM798"],method="spearman")
cor.test(donTM498$Age,donTM498$Income,method="spearman")
cor.test(donTM195$Age,donTM195$Income,method="spearman")

#avec la fonction summarise
don %>% group_by(Product) %>%
  summarise(
    n = n(),
    r_pearson = cor(Age, Income, method="pearson"),
    p_pearson = cor.test(Age, Income)$p.value,
    r_spearman = cor(Age, Income, method="spearman"),
    p_spearman = cor.test(Age, Income, method="spearman")$p.value
  )





# Question 6 ----

# Sélection des variables quanti continues
quant_vars <- don%>%
  select(Age,Income,Miles) 

# Matrice de corrélation
cor_matrix <- cor(quant_vars, method="spearman",use = "complete.obs")
cor_matrix

# Question 6.a ----

# Visualisation de la matrice de corrélation avec corrplot
corrplot(cor_matrix)
corrplot(cor_matrix,type="upper")
corrplot(cor_matrix, addCoef.col = "black", number.cex = 0.7)

corrplot(cor_matrix,type="upper", addCoef.col = "black", number.cex = 0.7)

# Question 6.b ----

#tous les nuages de points avec la fonction pairs
pairs(quant_vars)
#pour quel couple de variable le lien est il significatif?

#question 6.c ----
for (p in levels(don$Product)){
  cat("Product:", p, "\n")
  sub <- don %>% filter(Product == p) %>% select(Age, Usage, Income, Miles)
  print(cor(sub, use="pairwise.complete.obs",method="spearman"))
  corrplot(cor(sub, use="pairwise.complete.obs",method="spearman"), main = paste("Correlations -", p))
}

#par type de produit, miles et income
#graphique et test

ggplot(don, aes(x = Income, y = Miles, color = Product)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ Product) +
  labs(
    title = "La répartition de la distance parcourue attendue en moyenne (Miles) par semaine en fonction du revenu annuel en dollar par type de produit",
    x = "Revenu annuel en dollar",
    y = "Distance (Miles)",
    color = "Produit"
  )



ggplot(don, aes(x = Income, y = Miles, color = Product)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(
    title = "Relation entre Income et Miles par type de produit",
    x = "Revenu",
    y = "Miles parcourus"
  )


cor.test(donTM195$Miles,donTM195$Income,method="spearman")
cor.test(donTM498$Miles,donTM498$Income,method="spearman")
cor.test(donTM798$Miles,donTM798$Income,method="spearman")


#par type de produit, age et miles


ggplot(don, aes(x = Age, y = Miles, color = Product)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ Product) +
  labs(
    title = "La répartition de la distance parcourue attendue en moyenne (Miles) par semaine en fonction de l'âge par type de produit",
    x = "Âge",
    y = "Distance (Miles)",
    color = "Produit"
  )

cor.test(donTM195$Age,donTM195$Miles,method="spearman")
cor.test(donTM498$Age,donTM498$Miles,method="spearman")
cor.test(donTM798$Age,donTM798$Miles,method="spearman")




cor.test(don$Age,don$Income,method="spearman")
cor.test(don$Age,don$Miles,method="spearman")
cor.test(don$Income,don$Miles,method="spearman")

cor.test(don$Age,don$Income,method="pearson")
cor.test(don$Age,don$Miles,method="pearson")
cor.test(don$Income,don$Miles,method="pearson")






#question 7. ----
#question 7.a ----
#comparaison TM195 et TM498
#creation base de donnees clients TM195 et TM498
donsans798<-don%>%
  filter(Product!="TM798")%>%
  mutate(Product=droplevels(Product))
summary(donsans798)
#ou  bien donsans798<-don%>%filter(Product %in% c("TM195","TM498"))

#statdes var quanti par produit
by(num_data,don$Product,summary)

#revenu par produit     
#statdes+graphique
#test

#statdes
donsans798%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Income,na.rm=T),
    sd = sd(Income,na.rm=T),
    q1=quantile(Income,prob=0.25),
    Med=quantile(Income,prob=0.5),
    q3=quantile(Income,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans798,aes(Product,Income))+
  geom_boxplot()+
  labs(title = "La répartition du revenu annuel en dolar selon le modèle de tapis acheté", x = "Modèle de tapis acheté", y = "Revenu annuel (en dollar)")+
  geom_hline(yintercept=mean(donTM195$Income),col="red")+
  geom_hline(yintercept=mean(donTM498$Income),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Income,donTM195$Income)
#p-value = 0.09279 on ne rejette pas H0 car le revenu ne depend pas significativement du produit





#Age par produit
# Résumé statistique

#statdes
donsans798%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Age,na.rm=T),
    sd = sd(Age,na.rm=T),
    q1=quantile(Age,prob=0.25),
    Med=quantile(Age,prob=0.5),
    q3=quantile(Age,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans798,aes(Product,Age))+
  geom_boxplot()+
  labs(title = "La répartition de l'age en fonction du modèle du tapis acheté", x = "Modèle du tapis acheté", y = "Age")+
  geom_hline(yintercept=mean(donTM195$Age),col="red")+
  geom_hline(yintercept=mean(donTM498$Age),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Age,donTM195$Age)
#p-value = 0.7669 on ne rejette pas H0 car l'Age ne depend pas significativement du produit


#Miles
# Résumé statistique+graphique
# Test 


#statdes
donsans798%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Miles,na.rm=T),
    sd = sd(Miles,na.rm=T),
    q1=quantile(Miles,prob=0.25),
    Med=quantile(Miles,prob=0.5),
    q3=quantile(Miles,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans798,aes(Product,Miles))+
  geom_boxplot()+
  labs(title = "La répartition de la distance parcourue attendue en moyenne par semaine (Miles) en fonction du modèle du tapis acheté", x = "Modèle du tapis acheté", y = "Distance (Miles)")+
  geom_hline(yintercept=mean(donTM195$Miles),col="red")+
  geom_hline(yintercept=mean(donTM498$Miles),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Miles,donTM195$Miles)
#p-value = 0.3401 on ne rejette pas H0 car la distance ne depend pas significativement du produit



#Education
# Résumé statistique+graphique
# Test 

#statdes
donsans798%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Education,na.rm=T),
    sd = sd(Education,na.rm=T),
    q1=quantile(Education,prob=0.25),
    Med=quantile(Education,prob=0.5),
    q3=quantile(Education,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans798,aes(Product,Education))+
  geom_boxplot()+
  labs(title = "La répartition du nombre d'années d'études en fonction du modèle du tapis acheté", x = "Modèle du tapis acheté", y = "Nombre d'années d'études")+
  geom_hline(yintercept=mean(donTM195$Education),col="red")+
  geom_hline(yintercept=mean(donTM498$Education),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Education,donTM195$Education)
#p-value = 0.3401 on ne rejette pas H0 car la distance ne depend pas significativement du produit




#Usage
# Résumé statistique
# Résumé statistique+graphique
# Test 


#statdes
donsans798%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Usage,na.rm=T),
    sd = sd(Usage,na.rm=T),
    q1=quantile(Usage,prob=0.25),
    Med=quantile(Usage,prob=0.5),
    q3=quantile(Usage,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans798,aes(Product,Usage))+
  geom_boxplot()+
  labs(title = "La répartition du nombre moyen d'utilisation prévu par semaine en fonction du modèle du tapis acheté", x = "Modèle du tapis acheté", y = "Nombre moyen d'utilisation")+
  geom_hline(yintercept=mean(donTM195$Usage),col="red")+
  geom_hline(yintercept=mean(donTM498$Usage),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Usage,donTM195$Usage)
#p-value = 0.8779 on ne rejette pas H0 car la distance ne depend pas significativement du produit




#question 7.b ----
#comparaison clients TM498 et 798 
donsans195<-don%>%
  filter(Product!="TM195")%>%
  mutate(Product=droplevels(Product))
summary(donsans195)

#statdes var quanti par produit

#revenu par produit     
#statdes+graphique
#test


#statdes
donsans195%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Income,na.rm=T),
    sd = sd(Income,na.rm=T),
    q1=quantile(Income,prob=0.25),
    Med=quantile(Income,prob=0.5),
    q3=quantile(Income,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans195,aes(Product,Income))+
  geom_boxplot()+
  labs(title = "La répartition du revenu annuel (dollar) en fonction du modèle du tapis acheté", x = "Modèle du tapis acheté", y = "Revenu annuel en dollar")+
  geom_hline(yintercept=mean(donTM798$Income),col="red")+
  geom_hline(yintercept=mean(donTM498$Income),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Income,donTM798$Income)
#p-value = 3.123e-11 on  rejette  H0 car le revenu  depend  significativement du produit





#Age par produit
# Résumé statistique


#statdes
donsans195%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Age,na.rm=T),
    sd = sd(Age,na.rm=T),
    q1=quantile(Age,prob=0.25),
    Med=quantile(Age,prob=0.5),
    q3=quantile(Age,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans195,aes(Product,Age))+
  geom_boxplot()+
  labs(title = "La répartition de l'âge en fonction du modèle du tapis acheté", x = "Modèle du tapis acheté", y = "Âge")+
  geom_hline(yintercept=mean(donTM798$Age),col="red")+
  geom_hline(yintercept=mean(donTM498$Age),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Age,donTM798$Age)
#p-value = 0.8865 on ne rejette pas H0 car l'Age ne depend pas significativement du produit



#Miles
# Résumé statistique+graphique
# Test 



#statdes
donsans195%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Miles,na.rm=T),
    sd = sd(Miles,na.rm=T),
    q1=quantile(Miles,prob=0.25),
    Med=quantile(Miles,prob=0.5),
    q3=quantile(Miles,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans195,aes(Product,Miles))+
  geom_boxplot()+
  labs(title = "La répartition de la distance parcourue attendue en moyenne par semaine (en miles) en fonction du modèle acheté", x = "Modèle du tapis acheté", y = "Distance moyenne par semaine (Miles)")+
  geom_hline(yintercept=mean(donTM798$Miles),col="red")+
  geom_hline(yintercept=mean(donTM498$Miles),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Miles,donTM798$Miles)
#p-value = 4.268e-10 on rejette H0 car la distance  depend  significativement du produit


#Education
# Résumé statistique+graphique
# Test 

#statdes
donsans195%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Education,na.rm=T),
    sd = sd(Education,na.rm=T),
    q1=quantile(Education,prob=0.25),
    Med=quantile(Education,prob=0.5),
    q3=quantile(Education,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans195,aes(Product,Education))+
  geom_boxplot()+
  labs(title = "La répartition du nombre d'années d'études en fonction du modèle du tapis acheté", x = "Modèle du tapis acheté", y = "Nombre d'années d'études")+
  geom_hline(yintercept=mean(donTM798$Education),col="red")+
  geom_hline(yintercept=mean(donTM498$Education),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Education,donTM798$Education)
#p-value = 4.268e-10 on rejette H0 car la distance depend significativement du produit





#Usage
# Résumé statistique
# Résumé statistique+graphique
# Test 

#statdes
donsans195%>%group_by(Product)%>%
  summarize(
    nb = n(),
    moy = mean(Usage,na.rm=T),
    sd = sd(Usage,na.rm=T),
    q1=quantile(Usage,prob=0.25),
    Med=quantile(Usage,prob=0.5),
    q3=quantile(Usage,prob=0.75)
  )


#boxplot par genre à commenter
ggplot(data=donsans195,aes(Product,Usage))+
  geom_boxplot()+
  labs(title = "La répartition du nombre moyen d'utilisation prévu par semaine en fonction du modèle du produit acheté", x = "Modèle du produit acheté", y = "Nombre moyen d'utilisation")+
  geom_hline(yintercept=mean(donTM798$Usage),col="red")+
  geom_hline(yintercept=mean(donTM498$Usage),col="blue")


#test statistique  : 2 grands ech, var inconue sans supposer egalite variance
#HO:muF=muH H1:muf diff muH, on recupere la pvaleur du cas t.test car echantillons grands

t.test(donTM498$Usage,donTM798$Usage)
#p-value = 3.076e-14 on ne rejette pas H0 car la distance ne depend pas significativement du produit


