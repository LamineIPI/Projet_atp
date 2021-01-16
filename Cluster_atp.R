####Clustering sur fichier atp_matches####
### PB : Identification des facteurs permettant d’expliquer les retournements 
###de situation (gagner un match en 5 sets en étant mené 2 sets à 0).
###Ne prendre que les 5 sets (minimum) (donc minimum 19 caracteres)  + ( ceux de la forme ...-6, .. -6 , 6-... , 6-... , 6-...)
###DISTANCE DE GOVER AVEC FONCTION DAISY POUR PROBLEME A VARIABLE MIXTES

library(readr)
library(tidyverse)
library(tidyr)
library("stringr")
library('purrr')
library(NbClust)
library(cluster)
library(factoextra)

lst <- list.files(path = "data/")
lst_data <- grep(pattern = "^atp_matches_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("data/", x, sep = "")))
names(lst_tib) <- lst_names


atp_Cluster <- reduce(.x = lst_tib, .f = bind_rows)

atp_Cluster %>%
  mutate(surface = factor(surface, levels = names(sort(table(surface))), ordered = TRUE)) %>%
  #Toilettage en prévision de l'inclusion dans un rapport ou un graphique
  rename(Surface = surface) %>%
  mutate(Surface = case_when(Surface == "Clay" ~ "Terre battue",
                             Surface == "Grass" ~ "Herbe",
                             Surface == "Hard" ~ "Dur")) -> atp_Cluster




atp_Cluster <- atp_Cluster[,c(15,16,23,24,27:45)]
atp_Cluster <- drop_na(atp_Cluster)
atp_Cluster <- filter(atp_Cluster, str_count(atp_Cluster$score,"-") == 5)
atp_Cluster <- filter(atp_Cluster, str_sub(atp_Cluster$score,1,1) <= str_sub(atp_Cluster$score,3,3)) 
atp_Cluster <- filter(atp_Cluster, str_sub(atp_Cluster$score,5,5) <= str_sub(atp_Cluster$score,7,7)) 
atp_Cluster2 <- atp_Cluster[,-4]


summary(atp_Cluster2)
str(atp_Cluster2)
head(atp_Cluster2,n = 4)

pairs(atp_Cluster2) 
atp_Cluster2_centré<- scale(atp_Cluster2)
dist <-  dist(atp_Cluster2_centré)
dist
CAH <- hclust(dist)
CAH2 <- hclust(dist,method ="ward.D2")
print(CAH)
print(CAH2)

#print(clu)
plot(CAH,main = "Dendrogramme ")
rect.hclust(CAH, k=2)#Hauteur de coupe 3 pour prendre 
plot(CAH2,main = "Dendrogramme en Ward.D2")
rect.hclust(CAH2, k=2)#Hauteur de coupe 3 pour prendre 
#On le regarde de bas en haut,attention ici height ne veut pas dire grand chose c'est juste la hauteur
#D'autre choix d'agregation DBSCAN, SpectralClustering, OPTICS, Birch,GaussianMixture etc DBSCAN VRAIMENT PROPRE
clu <- cutree(CAH2,k=3)
pairs(atp_Cluster2_centré, pch=21,bg=clu)#Une acp aurait été utile avant

atp_Cluster2k <- kmeans(atp_Cluster2_centré,4)
fviz_cluster(atp_Cluster2k, data = atp_Cluster2_centré,
             palette = c("#2E9FDF","#00AFBB","#E78800","#e74500"),
             geom ="point",
             ellipse.type = "convex",#ou norm (si repartition gaussienne)
             ggtheme = theme_bw()
)

#nb opti de classe

# Elbow method
fviz_nbclust(atp_Cluster2_centré, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(atp_Cluster2_centré, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(atp_Cluster2_centré, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
#Pourquoi  ?? 
nb <- NbClust(atp_Cluster2_centré, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)

######################################### Caractéristique de remontada ###############################

atp_Cluster %>% 
  mutate(remontada = ifelse(str_count(score,"-") == 5,
                            ifelse(str_sub(score,1,1) <= str_sub(score,3,3),
                                  ifelse(str_sub(score,5,5) <= str_sub(score,7,7),1,0),0),0)
         ) %>% select(-score) -> atp_remontada


## Recherche de caractéristiques
library(MASS)
modele.complet <- glm(formula = as.factor(remontada) ~ ., family = binomial, data = atp_remontada)
modele.trivial <- glm(formula = as.factor(remontada) ~ 1, family = binomial, data = atp_remontada)

select.modele.bic.back <- step(object = modele.complet, 
                               scope = list(lower = modele.trivial, upper = modele.complet), 
                               direction = "backward", k = log(n))
