# Indiquer le dossier de travail
setwd("C:/Users/User/Desktop")

# Quelques packages de base pour traiter les donn�es
library(dplyr)
library(readxl)
library(tidyr)

# Importation du tableau
Inventaire <- read_excel("Inventaire monitoring 2023.xlsx")


# Identification � la famille quand le genre est inconnu :

Inventaire$Taxo = Inventaire$Genre_espece #On duplique la colonne qui porte le nom des esp�ces
Inventaire[Inventaire$Genre_espece == "_","Taxo"]<- Inventaire[Inventaire$Genre_espece == "_","Famille"] #On remplace tous les vides par les noms de famille


# Richesse sp�cifique par groupe taxonomique

Div<-Inventaire%>%filter(Station!="")  %>% group_by (Station,`Type de milieu`,`Groupe taxonomique` ) %>% summarise("Diversit�" = length(unique(Taxo)))%>% pivot_wider( names_from = "Groupe taxonomique",values_from = "Diversit�")%>% replace(is.na(.), 0)

# Pr�sence absence, pour tous les groupes

Communaute <-Inventaire%>%filter(Station!="")  %>% group_by (Station,`Type de milieu`, Taxo) %>% summarise("Pr�sence" = 1) %>% pivot_wider( names_from = "Taxo",values_from = "Pr�sence")%>% replace(is.na(.), 0)

# Indices de diversit�

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