########## PREPARATION DU CODE

### Effacer les objets de l'environnement
rm(list = ls())

### Bibliotheques
library(readr)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(purrr)
library(readxl)
library(questionr)
library(ggplot2)




########## PRÉPARATION DES DONNÉES

### Import des données
hosp <- read_delim("data/hosp.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
hosp <- hosp[,-c(6)]
vaccin <- read_delim("data/vaccin.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)
pop <- read_excel("data/pop.xlsx", 
                  sheet = "2021", skip = 4)
pop <- pop[,c(1,3:8)]
pop <- rename.variable(pop, c("...1","0 à 19 ans...3","20 à 39 ans...4","40 à 59 ans...5",
                              "60 à 74 ans...6","75 ans et plus...7","Total...8"),
                       c("dep","age_0_19ans","age_20_39ans","age_40_59ans","age_60_74ans",
                         "age_75+","pop"))

### Mise en forme des bases selon les departements + total
hosp %>%
  group_by(dep) %>%
  filter(jour!="2021-12-20") %>%
  summarise(hosp=sum(incid_hosp),
            rea=sum(incid_rea),
            dc=sum(incid_dc)) -> hosp
vaccin %>%
  filter(jour=="2021-12-19") %>%
  select(dep,n_cum_dose1,n_cum_complet,n_cum_rappel) -> vaccin
vaccin$dose1 <- vaccin$n_cum_dose1 - vaccin$n_cum_complet
vaccin$dose2 <- vaccin$n_cum_complet - vaccin$n_cum_rappel
vaccin$dose3 <- vaccin$n_cum_rappel
vaccin <- vaccin[,-c(2,3,4)]

### Jointure des bases
data <- right_join(hosp,vaccin,by="dep")
data <- left_join(data,pop,by="dep")
write_csv(data,"/Users/hugocarlin/Documents/Université/Master SEP/M2/S1/SEP0931 - Analyse de données/Projets/Projet final/Projet_final_ADD_HugoCarlin_AyoubBridaoui/data/data.csv")

### Format des variables
summary(data)
data$dep <- factor(data$dep)

### Ajout de variables
data$non_vacc <- data$pop - data$dose1 - data$dose2 - data$dose3

### Nouvelle base avec le taux d'indicdence
data_freq <- data
data_freq$hosp <- data_freq$hosp/data_freq$pop*100000
data_freq$rea <- data_freq$rea/data_freq$pop*100000
data_freq$dc <- data_freq$dc/data_freq$pop*100000
data_freq$dose1 <- data_freq$dose1/data_freq$pop*100000
data_freq$dose2 <- data_freq$dose2/data_freq$pop*100000
data_freq$dose3 <- data_freq$dose3/data_freq$pop*100000
data_freq$age_0_19ans <- data_freq$age_0_19ans/data_freq$pop*100000
data_freq$age_20_39ans <- data_freq$age_20_39ans/data_freq$pop*100000
data_freq$age_40_59ans <- data_freq$age_40_59ans/data_freq$pop*100000
data_freq$age_60_74ans <- data_freq$age_60_74ans/data_freq$pop*100000
data_freq$`age_75+` <- data_freq$`age_75+`/data_freq$pop*100000
data_freq$non_vacc <- data_freq$non_vacc/data_freq$pop*100000

### Rownames
rownames(data) <- data$dep
rownames(data_freq) <- data_freq$dep





########## STATISTIQUES DESCRIPTIVES

### Complications
data_hosp <- data.frame(c("Hospitalisations","Réanimations","Décés"),
                        c(sum(data$hosp),sum(data$rea),sum(data$dc)))
colnames(data_hosp) <- c("statut","nb")
ggplot(data_hosp, aes(x=reorder(statut,-nb), y=nb, fill=statut)) +
  geom_col(fill = c("lightskyblue2","lightskyblue3","lightskyblue4")) +
  geom_text(aes(y=nb,label = paste(nb," (",round(nb/sum(data$pop)*100,2),"%)",sep="")), vjust = 1.6, color = "black") +
  labs(title = "Effectifs et fréquences des individus ayant subis des complications dûes au Covid-19",
       subtitle = "(en France métropolitaine le 19/12/2021, depuis le début de la pandémie)",
       x = "", y = "Effectif") +
  scale_y_continuous(breaks=seq(0,600000,by=100000),
                     labels=c("0","100 000","200 000","300 000","400 000","500 000","600 000"))

### Vaccin
data_vaccin <- data.frame(c("1 dose","2 doses","3 doses","non vaccinés"),
                        c(sum(data$dose1),sum(data$dose2),sum(data$dose3),sum(data$non_vacc)))
colnames(data_vaccin) <- c("dose","nb")
ggplot(data_vaccin, aes(x=reorder(dose,-nb), y=nb, fill=dose)) +
  geom_col(fill = c("darkolivegreen3","darkolivegreen2","darkolivegreen1","firebrick3")) +
  geom_text(aes(y=6000000,label = paste(nb,"\n(",round(nb/sum(data$pop)*100,2),"%)",sep="")), vjust = 1.6, color = "black") +
  labs(title = "Effectifs et fréquences des individus vaccinés contre le Covid-19",
       subtitle = "(en France métropolitaine le 19/12/2021, depuis le début de la pandémie)",
       x = "", y = "Effectif") +
  scale_y_continuous(breaks=seq(0,50000000,by=10000000),
                     labels=c("0","10 000 000","20 000 000","30 000 000","40 000 000","50 000 000"))

### Tranche d'age population
data_age <- data.frame(c("age_0_19ans","age_20_39ans","age_40_59ans","age_60_74ans","age_75+"),
                          c(sum(data$age_0_19ans),sum(data$age_20_39ans),sum(data$age_40_59ans),sum(data$age_60_74ans),sum(data$`age_75+`)))
colnames(data_age) <- c("age","nb")
ggplot(data_age, aes(x=reorder(age,-nb), y=nb, fill=age)) +
  geom_col(fill = c("lightblue","lightskyblue1","lightskyblue2","lightskyblue3","lightskyblue4")) +
  geom_text(aes(y=nb,label = paste(nb,"\n(",round(nb/sum(data$pop)*100,2),"%)",sep="")), vjust = 1.6, color = "black") +
  labs(title = "Effectifs et fréquences des tranches d'âge de la population",
       subtitle = "(en France métropolitaine en 2021)",
       x = "", y = "Effectif") +
  scale_y_continuous(breaks=seq(0,15000000,by=5000000),
                     labels=c("0","5 000 000","10 000 000","15 000 000"))





########## ACP

### ACP générale
res.PCA<-PCA(data_freq,quali.sup=c(1),quanti.sup=c(13),graph=FALSE)
# Qualité
fviz_eig(res.PCA, addlabels = TRUE, ylim = c(0, 50))
# Variables
plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
# Individus
plot.PCA(res.PCA,invisible=c('quali','ind.sup'),title="Graphe des individus de l'ACP",label =c('ind'))

### ACP vaccin et hospitalisation
res.PCA<-PCA(data_freq,quali.sup=c(1),quanti.sup=c(8,9,10,11,12,13),graph=FALSE)
# Qualité
fviz_eig(res.PCA, addlabels = TRUE, ylim = c(0, 50))
# Variables
plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
# Individus
plot.PCA(res.PCA,invisible=c('quali','ind.sup'),title="Graphe des individus de l'ACP",label =c('ind'))

### ACP age et vaccin
res.PCA<-PCA(data_freq,quali.sup=c(1),quanti.sup=c(2,3,4,13),graph=FALSE)
# Qualité
fviz_eig(res.PCA, addlabels = TRUE, ylim = c(0, 50))
# Variables
plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
# Individus
plot.PCA(res.PCA,invisible=c('quali','ind.sup'),title="Graphe des individus de l'ACP",label =c('ind'))

### ACP age et hospitalisations
res.PCA<-PCA(data_freq,quali.sup=c(1),quanti.sup=c(5,6,7,13,14),graph=FALSE)
# Qualité
fviz_eig(res.PCA, addlabels = TRUE, ylim = c(0, 60))
# Variables
plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
# Individus
plot.PCA(res.PCA,invisible=c('quali','ind.sup'),title="Graphe des individus de l'ACP",label =c('ind'))


