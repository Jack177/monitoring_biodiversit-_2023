# Indiquer le dossier de travail
setwd("C:/Users/User/Desktop")

# Quelques packages de base pour traiter les donnees
library(dplyr)
library(readxl)
library(tidyr)

# Importation du tableau
#Inventaire <- read_excel("Inventaire monitoring 2023.xlsx")

Inventaire <- df
# Identification e la famille quand le genre est inconnu :

Inventaire$Taxo = Inventaire$Genre_espece #On duplique la colonne qui porte le nom des especes
Inventaire[Inventaire$Genre_espece == "_","Taxo"]<- Inventaire[Inventaire$Genre_espece == "_","Famille"] #On remplace tous les vides par les noms de famille


# Richesse specifique par groupe taxonomique

Div<-Inventaire%>%filter(Station!="")  %>% group_by (Station,`Type de milieu`,`Groupe taxonomique` ) %>% summarise("Diversite" = length(unique(Taxo)))%>% pivot_wider( names_from = "Groupe taxonomique",values_from = "Diversite")%>% replace(is.na(.), 0)

# Presence absence, pour tous les groupes

Communaute <-Inventaire%>%filter(Station!="")  %>% group_by (Station,`Type de milieu`, Taxo) %>% summarise("Presence" = 1) %>% pivot_wider( names_from = "Taxo",values_from = "Presence")%>% replace(is.na(.), 0)

# Indices de diversite

Index <- Div[,1:2]
library(vegan)
SR <- rowSums(Communaute[,-c(1,2)])
shannon <- diversity(Communaute[,-c(1,2)], index = "shannon")
simpson <- diversity(Communaute[,-c(1,2)], index = "simpson")
Index <- cbind(Index,"SR"=SR,"shannon" =shannon,"simpson" =simpson)



# CA 
library(vegan)
res.ca = cca(Div[,-c(1,2)])
summary(res.ca)
screeplot(res.ca, bstick = TRUE)
plot(res.ca, scaling =1) 
plot(res.ca, scaling =2)
