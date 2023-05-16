# Indiquer le dossier de travail
setwd("C:/Users/User/Desktop")

# Quelques packages de base pour traiter les données
library(dplyr)
library(readxl)
library(tidyr)

# Importation du tableau
Inventaire <- read_excel("Inventaire monitoring 2023.xlsx")


# Identification à la famille quand le genre est inconnu :

Inventaire$Taxo = Inventaire$Genre_espece #On duplique la colonne qui porte le nom des espèces
Inventaire[Inventaire$Genre_espece == "_","Taxo"]<- Inventaire[Inventaire$Genre_espece == "_","Famille"] #On remplace tous les vides par les noms de famille


# Richesse spécifique par groupe taxonomique

Div<-Inventaire%>%filter(Station!="")  %>% group_by (Station,`Type de milieu`,`Groupe taxonomique` ) %>% summarise("Diversité" = length(unique(Taxo)))%>% pivot_wider( names_from = "Groupe taxonomique",values_from = "Diversité")%>% replace(is.na(.), 0)

# Présence absence, pour tous les groupes

Communaute <-Inventaire%>%filter(Station!="")  %>% group_by (Station,`Type de milieu`, Taxo) %>% summarise("Présence" = 1) %>% pivot_wider( names_from = "Taxo",values_from = "Présence")%>% replace(is.na(.), 0)

# Indices de diversité

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
