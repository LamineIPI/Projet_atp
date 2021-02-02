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
library('stringr')

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




a#on sélectionne juste les colonnes qui nous intéresse 

atp_Cluster <- atp_Cluster[,c(15,16,23,24,27:45)]
atp_Cluster <- drop_na(atp_Cluster)

# On créé atp_remontada_all qui est une base de données avec la variable en plus donnant la remontada ou non 0 ou 1
atp_Cluster %>% 
  mutate(remontada = ifelse(str_count(score,"-") == 5,
                            ifelse(str_sub(score,1,1) <= str_sub(score,3,3),
                                   ifelse(str_sub(score,5,5) <= str_sub(score,7,7),1,0),0),0)
  ) -> atp_remontada_all

#on récupère seulement les remontadas
atp_remontada_seul <- filter(atp_Cluster, str_count(atp_Cluster$score,"-") == 5)
atp_remontada_seul <- filter(atp_remontada_seul, str_sub(atp_remontada_seul$score,1,1) <= str_sub(atp_remontada_seul$score,3,3))
atp_remontada_seul <- filter(atp_remontada_seul, str_sub(atp_remontada_seul$score,5,5) <= str_sub(atp_remontada_seul$score,7,7))

#atp_cluster_test donne une bdd moitié remontada moitié matchs normaux
atp_Cluster %>% filter(atp_remontada_all$remontada == 0) -> atp_moit
atp_moit<- atp_moit[sample(1:nrow(atp_moit), 100, replace=FALSE), ]
atp_moit_rem <- atp_remontada_seul[sample(1:nrow(atp_remontada_seul), 100, replace=FALSE), ]

atp_cluster_test <- full_join(atp_moit,atp_moit_rem)
atp_cluster_test %>% 
  mutate(remontada = ifelse(str_count(score,"-") == 5,
                            ifelse(str_sub(score,1,1) <= str_sub(score,3,3),
                                   ifelse(str_sub(score,5,5) <= str_sub(score,7,7),1,0),0),0)
  ) -> atp_cluster_test
atp_cluster_test <- atp_cluster_test[,-4]

summary(atp_cluster_test)
str(atp_cluster_test)
head(atp_cluster_test,n = 4)

pairs(atp_cluster_test) 
atp_Cluster2_centré<- scale(atp_cluster_test)
dist <-  dist(atp_cluster_test)
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
#Le nombre optimal de cluster semble donc être de deux Clusters 

atp_Cluster2k <- kmeans(atp_Cluster2_centré,2)
fviz_cluster(atp_Cluster2k, data = atp_Cluster2_centré,
             geom ="point",
             ellipse.type = "convex",#ou norm (si repartition gaussienne)
             ggtheme = theme_bw()
)
# Atp sur le centrage réduction 

dist <-  dist(atp_Cluster2_centré)
dist
CAH <- hclust(dist)
CAH2 <- hclust(dist,method ="ward.D2")

plot(CAH,main = "Dendrogramme ")
rect.hclust(CAH, k=2)#Hauteur de coupe 3 pour prendre 
plot(CAH2,main = "Dendrogramme en Ward.D2")
rect.hclust(CAH2, k=2)#Hauteur de coupe 3 pour prendre 
#On le regarde de bas en haut,attention ici height ne veut pas dire grand chose c'est juste la hauteur
               
################ TREE ######################################

library('rpart') # For computing classification tree
library('rattle') # For displaying nicely classification trees prodiced by rpart
library('tree') #For growing a single tree and pruning it

#### Extracting training and test datasets ####
set.seed(234)
N <- nrow(atp_cluster_test)
sel <- sample(1:N, size = 25, replace = FALSE)
atp_cluster_test <- atp_cluster_test[,-c(1:3)]
atp_cluster_test <- atp_cluster_test[,-c(11:19)]
atp_train <- atp_cluster_test[setdiff(1:N, sel),]
atp_test <- atp_cluster_test[sel,]

#### Growing a tree with default parameters ####
atp_tree1 <- rpart(formula = remontada ~ .,
                   data = atp_train,
                    control = tree.control(nobs = nrow(atp_train), mindev = 0))
atp_tree1 # Displaying tree in console
fancyRpartPlot(model = atp_tree1) #A pretty representation of the classification tree

#A basic representation, but with branch length proportional to criteria loss 
plot(atp_tree1)
par(cex=0.7)
text(atp_tree1, pretty=0)

#### Growing a deep tree by adjusting control parameters ####
atp_tree2 <- rpart(formula = remontada ~ .,
                   data = atp_train,
                   control = rpart.control(minsplit = 2, cp = 0))
fancyRpartPlot(model = atp_tree2)


#### Bootstrap aggregating ####

## Loading package
library('randomForest')
## Preparing data
##removing incomplete data
# La fonction randomForest s'accomode mal des valeurs manquantes

# On renomme la variable `Suspicion diagnostic` dont le nom non standard semble poser probleme.
# On dÃ©clare adequation en tant que facteur pour obtenir des arbres de classification

set.seed(123)
randomForest(remontada ~  .,
             data = atp_remontada_all, 
             mtry =5,
             ntree = 500,
             importance = TRUE,
             keep.forest = TRUE) -> atp_bagging
predict(atp_bagging, newdata = atp_test) -> yhat
# Confusion matrix for bagging
table(atp_test$remontada, yhat) -> conf_mat
tx_err_bagging <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_bagging


#### Random forest ####
set.seed(123)
randomForest(remontada ~ .,
             data = atp_remontada_all, 
             mtry = 5,
             ntree = 500,
             importance = TRUE,
             keep.forest = TRUE) -> atp_rf
predict(atp_rf, newdata = atp_test) -> yhat
# Confusion matrix for random forest
table(atp_test$remontada, yhat) -> conf_mat
tx_err_rf <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_rf # DÃ©sastreux...

               
               
               
######################################### Caractéristique de remontada ###############################

## Recherche de caractéristiques
library(MASS)
modele.complet <- glm(formula = as.factor(remontada) ~ ., family = binomial, data = atp_remontada)
modele.trivial <- glm(formula = as.factor(remontada) ~ 1, family = binomial, data = atp_remontada)

select.modele.bic.back <- step(object = modele.complet, 
                               scope = list(lower = modele.trivial, upper = modele.complet), 
                               direction = "backward", k = log(n))
