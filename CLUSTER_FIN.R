####Clustering sur fichier atp_matches####
### PB : Identification des facteurs permettant d'expliquer les retournements 
###de situation (gagner un match en 5 sets en étant mené 2 sets à 0).
###Ne prendre que les 5 sets (minimum) (donc minimum 19 caracteres)  + ( ceux de la forme ...-6, .. -6 , 6-... , 6-... , 6-...)
###DISTANCE DE GOVER AVEC FONCTION DAISY POUR PROBLEME A VARIABLE MIXTES


##### Installation des packages #####

library(readr)
library(tidyverse)
library(tidyr)
library("stringr")
library('purrr')
library(NbClust)
library(cluster)
library(factoextra)
library('stringr')
library('randomForest')
library('rpart') # For computing classification tree
library('rattle') # For displaying nicely classification trees prodiced by rpart
library('tree') #For growing a single tree and pruning it
library(corrplot) 
library(MASS)
library(boot)
library(rmarkdown)

lst <- list.files(path = "data/")
lst_data <- grep(pattern = "^atp_matches_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("data/", x, sep = "")))
names(lst_tib) <- lst_names

################### Définition des bases de données #####################

atp_Cluster <- reduce(.x = lst_tib, .f = bind_rows)

atp_Cluster %>%
  mutate(surface = factor(surface, levels = names(sort(table(surface))), ordered = TRUE)) %>%
  #Toilettage en prévision de l'inclusion dans un rapport ou un graphique
  rename(Surface = surface) %>%
  mutate(Surface = case_when(Surface == "Clay" ~ "Terre battue",
                             Surface == "Grass" ~ "Herbe",
                             Surface == "Hard" ~ "Dur")) -> atp_Cluster



#on sélectionne juste les colonnes qui nous intéresse 

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

atp_Cluster %>% filter(atp_remontada_all$remontada == 0) -> atp_moit
atp_moit <- atp_moit[sample(1:nrow(atp_moit), 421, replace=FALSE), ]
atp_moit_rem <- atp_remontada_seul

atp_cluster_test <- full_join(atp_moit,atp_moit_rem)

atp_cluster_test %>% 
  mutate(remontada = ifelse(str_count(score,"-") == 5,
                            ifelse(str_sub(score,1,1) <= str_sub(score,3,3),
                                   ifelse(str_sub(score,5,5) <= str_sub(score,7,7),1,0),0),0)
  ) -> atp_cluster_test
atp_cluster_test <- atp_cluster_test[,-4]

#que les 5 sets et selection des variables pertinentes
atp_remontada_all  <- filter(atp_remontada_all, str_count(atp_remontada_all$score,"-") == 5)
atp_remontada_all  <- atp_remontada_all[,c(4:24)]



################################# Echantillonnage des donnees ########################################

####Extraire que les matchs 5 sets non remontada 
atp_remontada_all %>%
  filter(remontada == 0) -> atp_non_remontada 

atp_non_remontada <- atp_non_remontada[,-c(1,21)]

####Standardiser nos donnees + calcule de la distance euclidienne 
atp_non_remontada.scaled <- scale(atp_non_remontada, center = TRUE, scale = TRUE)
atp_non_remontada.dist <- dist(atp_non_remontada.scaled, "euclidean")

####Detection des anomalies avec DBSCAN 
#Detecter les valeurs aberrantes 
db <- fpc::dbscan(atp_non_remontada.scaled, eps =8, MinPts = 5)
fviz_cluster(db, atp_non_remontada.scaled, geom = "point")
#Extraire les valeurs aberrantes
pt_aberrant = which(db$isseed == FALSE)
atp_non_remontada.scaled_anomalie=atp_non_remontada.scaled[-pt_aberrant,]
db <- fpc::dbscan(atp_non_remontada.scaled_anomalie, eps =8, MinPts = 5)
#Visualisation des donnees sans valeurs aberrantes 
fviz_cluster(db, atp_non_remontada.scaled_anomalie, geom = "point")

##### Determination du K optimal (sans les valeurs aberrantes)

# Elbow method nous donne K = 2
fviz_nbclust(atp_non_remontada.scaled_anomalie, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

#Silhouette method nous donne K = 2 
fviz_nbclust(atp_non_remontada.scaled_anomalie, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#10 methodes proposent une repartition de 2 clusters contre 8 methodes qui recommandent
#une repartition de 3 clusters 
nbClust <- NbClust(atp_non_remontada.scaled_anomalie, distance = "euclidean", min.nc = 2,
                   max.nc = 10, method = "kmeans")
fviz_nbclust(nbClust)

# On choisit une repartition de 2 clusters #

#D'apres la visualisation du dendogramme, le CAH confirme le choix de 2 clusters
par(mfrow =c(1,1))
atp_non_remontada.dist_anomalie <- dist(atp_non_remontada.scaled_anomalie, "euclidean")
hca <- hclust(atp_non_remontada.dist_anomalie, method = "ward.D2")
plot(hca,main = "Dendrogramme ",xlab = "Matchs des 5 sets sans remontada")
rect.hclust(hca, k=2)

####Affichage des clusters par l'algorithme du K-Medoids
# On prend K = 2
kmed <- pam(atp_non_remontada.scaled_anomalie, 2, metric = "euclidean", stand = TRUE)
# Visualisation
fviz_cluster(kmed,data = atp_non_remontada.scaled_anomalie,
             palette = c("#2E9FDF", "#00AFBB"),   
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw())
#enlever les valeurs aberrantes de notre base de depart
atp_non_remontada <- atp_non_remontada[-pt_aberrant,]
#Ajouter la variable cluster, Nombre d'individus par cluster et 
#la proportion d'individus dans chaque cluster 
atp_non_remontada=cbind(atp_non_remontada,cluster=kmed$clustering)
atp_non_remontada %>%
  group_by(cluster) %>%
  summarize(Nb_ind = n()) %>%
  mutate(Tau_ind =Nb_ind /sum(Nb_ind)) -> taux_indiv_clus

#Creation d'une base (de non remontada) de 421 observations dont lequel chaque 
#cluster va etre representer avec la meme proportion que la base 
#initiale(on a pris que les 5 sets) 

atp_non_remontada %>%
  filter(cluster == 1) %>%
  mutate(remontada= rep(0,each=1002)) -> atp_non_remontada.1
atp_non_remontada.1 <- atp_non_remontada.1[sample(1:nrow(atp_non_remontada.1), 
                                                  (421*taux_indiv_clus$Tau_ind[1]+1), replace=FALSE), -20]

atp_non_remontada %>%
  filter(cluster == 2) %>%
  mutate(remontada= rep(0,each=1177)) -> atp_non_remontada.2
atp_non_remontada.2 <- atp_non_remontada.2[sample(1:nrow(atp_non_remontada.2), 
                                                  (421*taux_indiv_clus$Tau_ind[2]), replace=FALSE),-20]

#Extraire les remontadas
atp_remontada_all %>%
  filter(remontada == 1)-> atp_remontada
atp_remontada <- atp_remontada[,-1]
#Fusion de remontada et non remontada
Echantillon_atp <- rbind(atp_remontada,atp_non_remontada.1,atp_non_remontada.2)

#### Création de la base d'entrainement et de test pour la sélection aléatoire ####
set.seed(234)
N <- nrow(atp_cluster_test)
sel <- sample(1:N, size = N*0.2, replace = FALSE)
atp_cluster_test <- atp_cluster_test[,-c(1:3)]
atp_train <- atp_cluster_test[setdiff(1:N, sel),]
atp_test <- atp_cluster_test[sel,]

#### Création de la base d'entrainement et de test pour la sélection par échantillonage ####
N <- nrow(Echantillon_atp)
sel <- sample(1:N, size = N*0.2, replace = FALSE)
atp_echant_train <- Echantillon_atp[setdiff(1:N, sel),]
atp_echant_test <- Echantillon_atp[sel,]

######################################## Création du Rmarkdown ####################################

render("Remontada_rapport.rmd")

######################### NE PAS LANCER LE CODE APRES CETTE LIGNE #########################################


################ TREE ######################################


#### Random Forest ####

set.seed(123)
randomForest(remontada ~ .,
             data = atp_train, 
             mtry = 4,
             ntree = 500,
             na.action = na.roughfix) -> atp_rf
atp_rf

# mean of squared residuals  = 0.0388 not that bad
predict(atp_rf, newdata = atp_test) -> yhat
# Confusion matrix for bagging
table(atp_test$remontada, yhat) -> conf_mat
tx_err_bagging <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_bagging

atp_rf$importance



#### Random Forest grâce à l'échantillonage ####

set.seed(123)
randomForest(remontada ~ .,
             data = atp_echant_train, 
             mtry = 4,
             ntree = 500,
             na.action = na.roughfix) -> atp_echant_rf
atp_echant_rf

# mean of squared residuals  = 0.0388 not that bad
predict(atp_echant_rf, newdata = atp_echant_test) -> yhat
# Confusion matrix for bagging
table(atp_echant_test$remontada, yhat) -> conf_echant_mat
tx_err_echant_RF <-(conf_mat[1,2] + conf_echant_mat[2,1]) / sum(conf_echant_mat)
tx_err_echant_RF

atp_echant_rf$importance




######################################### Regréssion logistique ###############################

## Recherche de caractéristiques

modele.complet <- glm(formula = as.factor(remontada) ~ ., family = binomial, data = atp_cluster_test)
modele.trivial <- glm(formula = as.factor(remontada) ~ 1, family = binomial, data = atp_cluster_test)
summary(modele.complet) #Beaucoup de variables ne sont pas significatives et cele peut etre du par des
#problème de multicolinéarité etre les variables explicatives. 


mcor = cor(atp_cluster_test[,-20])
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45) 
#D'après la matrice de corrélation, on remarque que plusieurs variables sont fortement corrélées. 
#Ainsi, pour palier à ce problème nous allons une sélection de variables selon le critère d'information de AIC.

# Faisons une selection des variables qui expliquent au mieux la variable cible (remontada) solon le 
# critère d'AIC.
select.modele <- step(object = modele.complet, 
                      scope = list(lower = modele.trivial, upper = modele.complet), 
                      direction = "backward")

select.modele
modele.optimal = formula(select.modele$model) #le modèle optimal obtenu

#### Test de validité du modèle global : H_0 : w_2 = w_3 = (0,0,0)
modele.RL <- glm(formula = modele.optimal, family = binomial, data = atp_cluster_test, maxit = 3000)
res <- summary(modele.RL)
res

#### Tester (avec rapport de vraismeblance) la validité du modéle complet 
Sn = modele.RL$null.deviance - modele.RL$deviance #la statistique du rapport de vraisemblance
print(Sn)
ddl = modele.RL$df.null - modele.RL$df.residual #nombre de degrés de liberté de la loi limite de Sn, sous H_0
print(ddl)
pvalue = pchisq(q = Sn, df = ddl, lower.tail = F) #p_value du test : P(Z>Sn) où Z suit une loi du chi^2(ddl)
print(pvalue) #on obtient 1.794716e-07, on rejette H0, donc le modèle est "trés" significatif


# Estimation de l'erreur de classification, par K-fold cross-validation 
# On peut utiliser le fonction cv.glm du package boot

modele.glm <- glm(formula = modele.optimal, family = binomial, data = atp_cluster_test, maxit = 3000)
cout <- function(r, pi) mean(abs(r-pi) > 0.5) #la fonction de cout, ce choix est approprié au cas d'une variable réponse binaire
# Par exemple K = 10, on obtient
K <- 10
cv.err <- cv.glm(data = atp_cluster_test, glmfit = modele.glm, cost = cout, K = K)
cv.err$delta[1] # un taux d'erreur de 0.42 : pas trop fameux ce taux d'erreur!
