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




#on sélectionne juste les colonnes qui nous intéresse 

atp_Cluster <- atp_Cluster[,c(15,16,23,24,27:45)]
atp_Cluster <- drop_na(atp_Cluster)

# On créé atp_remontada_all qui est une base de données avec la variable en plus donnant la remontada ou non 0 ou 1
atp_Cluster %>% 
  mutate(remontada = ifelse(str_count(score,"-") == 5,
                            ifelse(str_sub(score,1,1) <= str_sub(score,3,3),
                                   ifelse(str_sub(score,5,5) <= str_sub(score,7,7),1,0),0),0)
  ) -> atp_remontada_all

#que les 5 sets et selection des variables pertinentes
atp_remontada_all  <- filter(atp_remontada_all, str_count(atp_remontada_all$score,"-") == 5)
atp_remontada_all  <- atp_remontada_all[,c(4:24)]

#selection de la base non remontada
atp_remontada_all %>% filter(atp_remontada_all$remontada == 0) -> atp_moit
#Prendre le méme nombre de ligne pr les deux base (Remontada et non remontada)
set.seed(123)
atp_moit <- atp_moit[sample(1:nrow(atp_moit), 421, replace=FALSE), ]
atp_moit_rem <- atp_remontada_all%>%filter(remontada==1)

atp_cluster_test <- full_join(atp_moit,atp_moit_rem)
#atp_cluster_test <- atp_cluster_test[,-1]

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
set.seed(123)
N <- nrow(atp_cluster_test)
sel <- sample(1:N, size = 253, replace = FALSE)
atp_cluster_test <- atp_cluster_test[,-1]
atp_train <- atp_cluster_test[setdiff(1:N, sel),]
atp_test <- atp_cluster_test[sel,]
               
atp_train %>% mutate(remontada =factor(remontada) ) -> atp_train
atp_test %>% mutate(remontada =factor(remontada) ) -> atp_test2


#### Growing a deep tree by adjusting control parameters ####
atp_tree2 <- rpart(formula = remontada ~ .,
                   data = atp_train,method = 'class',
                   control = rpart.control(minsplit = 2, cp = 0.005, maxdepth = 30))
#fancyRpartPlot(model = atp_tree2)


##Prediction on training set
test_pred_atp <- predict(atp_tree2, atp_train, type='class')
table(test_pred_atp, atp_train$remontada) -> test_classification
#misclassification rate :
(test_classification[1,2] + test_classification[2,1])/sum(test_classification)
# = 0.1256367 ok erreur faible (normal car train je pense)

#Class prediction on testing set
test_pred_atp <- predict(atp_tree2, atp_test,type='class')
table(test_pred_atp, atp_test$remontada) -> test_classification
#misclassification rate :
(test_classification[1,2] + test_classification[2,1])/sum(test_classification)
# = 0.4743083 ah ! bien haut

#### Prunning tree ####
## Cross validation using xpred.rpart

xpred.rpart(atp_tree2, xval = 10, return.all = FALSE) -> cv_results
str(cv_results)
#Le résultat est un tableau à trois dimensions. 
cv_results #Les prédictions par validation croisée pour chaque valeur du paramètre
atp_train$remontada 

## Pour choisir le paramètre cp donnant la meilleure prédiction par validation croisée, on calcule le taux de mauvaise classification pour chaque valeur
# Les modalités de la variable cible sont codées 1 et 0 ; le codage est donné dans la liste des attributs de rpu_pruned_tree
ylev <- attributes(atp_tree2)$ylevels
cv_results
#Calcul du taux de mauvaise classification pour chaque valeur de alpha/alpha_max
tx_err_vc <- apply(cv_results, 2, function(x) sum(ylev[x] != atp_train$remontada)/ nrow(atp_train))
cp_opt <- as.numeric(names(tx_err_vc)[which.min(tx_err_vc)])
atp_pruned_tree <- prune.rpart(tree = atp_tree2, cp = cp_opt)

#fancyRpartPlot(atp_pruned_tree)

#########################
# Prediction on the testing set set
pred_atp_pruned <- predict(atp_pruned_tree, atp_test, type='class')
table(pred_atp_pruned, atp_test$remontada) -> test_classification
#misclassification rate :
(test_classification[1,2] + test_classification[2,1])/sum(test_classification)
# 0.4664032
#Erreur catastrophique
# Essayons avec d'autre variables, pour trouver lesquels utiliser => Bagging
  
####Bagging et obtention importance des variables
atp_train %>% mutate(remontada =factor(remontada) ) -> atp_train2
atp_test %>% mutate(remontada =factor(remontada) ) -> atp_test2
set.seed(123)
randomForest(remontada ~ ., 
             data = atp_train2, 
             mtry = 19,
             ntree = 500,
             importance = TRUE,
             keep.forest = TRUE) -> atp_bagging
predict(atp_bagging, newdata = atp_test2) -> yhat

#Matrice de confusion
table(atp_test2$remontada, yhat) -> conf_mat
tx_err_bagging <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_bagging 
#On a un taux d'erreur de 47.43083%

## Lecture de quelques resultats
# Matrice de confusion, avec le taux d'erreur de chaque modalite de mo
atp_bagging$confusion

# Importance des variables
atp_bagging$importance
varImpPlot(atp_bagging)

#On va donc garder les variables : minutes, w_ace , w_svpt , w_1stWon , l_ace , l_svpt , w_2ndWon , w_bpFaced , l_2ndWon , l_1stWon , l_df , w_df , w_bpSaved
#on refait avec les variables les + importante (Gini)

set.seed(123)
atp_tree3 <- rpart(formula = remontada ~  minutes + w_ace + w_svpt + w_1stWon + l_ace + l_svpt + w_2ndWon + w_bpFaced + l_2ndWon + l_1stWon + l_df + w_df + w_bpSaved    ,
                   data = atp_train,method = 'class',
                   control = rpart.control(minsplit = 2, cp = 0.005, maxdepth = 30))

#fancyRpartPlot(atp_tree3)
## Cross validation using xpred.rpart
xpred.rpart(atp_tree3, xval = 10, return.all = FALSE) -> cv_results
str(cv_results)
cv_results #Les prédictions par validation croisée pour chaque valeur du paramètre
atp_train$remontada 

## Pour choisir le paramètre cp donnant la meilleure prédiction par validation croisée, on calcule le taux de mauvaise classification pour chaque valeur
# Les modalités de la variable cible sont codées 1 et 0 ; le codage est donné dans la liste des attributs de atp_pruned_tree
ylev <- attributes(atp_tree3)$ylevels
cv_results
#Calcul du taux de mauvaise classification pour chaque valeur de alpha/alpha_max
tx_err_vc <- apply(cv_results, 2, function(x) sum(ylev[x] != atp_train$remontada)/ nrow(atp_train))
cp_opt <- as.numeric(names(tx_err_vc)[which.min(tx_err_vc)])
atp_pruned_tree <- prune.rpart(tree = atp_tree3, cp = cp_opt)

#fancyRpartPlot(atp_pruned_tree)

#########################
# Prediction on the testing set set
pred_atp_pruned <- predict(atp_pruned_tree, atp_test, type='class')
table(pred_atp_pruned, atp_test$remontada) -> test_classification
#misclassification rate :
(test_classification[1,2] + test_classification[2,1])/sum(test_classification)
#0.4782609... pas fou ça mais mieux
                   
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
modele.complet <- glm(formula = as.factor(remontada) ~ ., family = binomial, data = atp_cluster_test)
modele.trivial <- glm(formula = as.factor(remontada) ~ 1, family = binomial, data = atp_cluster_test)
summary(modele.complet) #Beaucoup de variables ne sont pas significatives et cele peut etre du par des
#problème de multicolinéarité etre les variables explicatives. 

library(corrplot) 
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

#### Test de validité du modèle global : H_0 : w_2 = w_3 = (0,0,0) ####
modele.RL <- glm(formula = modele.optimal, family = binomial, data = atp_cluster_test, maxit = 3000)
res <- summary(modele.RL)
res

#### Tester (avec rapport de vraismeblance) la validité du modéle complet ####
Sn = modele.RL$null.deviance - modele.RL$deviance #la statistique du rapport de vraisemblance
print(Sn)
ddl = modele.RL$df.null - modele.RL$df.residual #nombre de degrés de liberté de la loi limite de Sn, sous H_0
print(ddl)
pvalue = pchisq(q = Sn, df = ddl, lower.tail = F) #p_value du test : P(Z>Sn) où Z suit une loi du chi^2(ddl)
print(pvalue) #on obtient 1.794716e-07, on rejette H0, donc le modèle est "trés" significatif


# Estimation de l'erreur de classification, par K-fold cross-validation 
# On peut utiliser le fonction cv.glm du package boot
library(boot)
modele.glm <- glm(formula = modele.optimal, family = binomial, data = atp_cluster_test, maxit = 3000)
cout <- function(r, pi) mean(abs(r-pi) > 0.5) #la fonction de cout, ce choix est approprié au cas d'une variable réponse binaire
# Par exemple K = 10, on obtient
K <- 10
cv.err <- cv.glm(data = atp_cluster_test, glmfit = modele.glm, cost = cout, K = K)
cv.err$delta[1] # un taux d'erreur de 0.42 : pas trop fameux ce taux d'erreur!

################################# Echantillonnage des donnees ########################################

####Extraire que les matchs 5 sets non remontada 
atp_remontada_all %>%
  filter(remontada == 0) %>% 
  select(-c(1,21)) -> atp_non_remontada

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

####### On choisit une repartition de 2 clusters ########
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
  filter(remontada == 1) %>%
  select(-c(1))-> atp_remontada
#Fusion de remontada et non remontada
Echantillon_atp <- rbind(atp_remontada,atp_non_remontada.1,atp_non_remontada.2)

####################################" BOOSTING and Bagging ###################"
######Bagging 
library(randomForest)
atp_cluster_test$remontada=as.factor(atp_cluster_test$remontada)
#train test split
set.seed(123)
index=sample(1:nrow(atp_cluster_test),0.7*nrow(atp_cluster_test),replace = FALSE)
train=atp_cluster_test[index,]
test=atp_cluster_test[-index,]
dim(train); dim(test)

#verification de l'equilibre au niveau de la variable cible 
train%>%group_by(remontada)%>%summarise(N=n())

#le bagging
set.seed(123)
randomForest(remontada ~ .,
             data = train, 
             mtry = 19,
             ntree = 500,
             maxnodes=20,
             importance = TRUE,
             keep.forest = TRUE) -> atp_bagging
predict(atp_bagging, newdata = test) -> yhat
# Confusion matrix for bagging
table(test$remontada, yhat) -> conf_mat
tx_err_bagging <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_bagging


# la matrice de confusion sur le train
atp_bagging$confusion

#donc on a un taux d'erreur sur l echontillon test de 0.47 et sur le train la moitiée qui font rémontada sont mal classé


# Importance des variables
atp_bagging$importance
varImpPlot(atp_bagging)




#selectionnant les meilleurs variables en terme de descente de l'Indice de GINI



set.seed(123)
index=sample(1:nrow(atp_cluster_test),0.7*nrow(atp_cluster_test),replace = FALSE)
train=atp_cluster_test[index,c("minutes","w_svpt","l_svpt","w_1stWon","remontada")]
test=atp_cluster_test[-index,c("minutes","w_svpt","l_svpt","w_1stWon","remontada")]
dim(train); dim(test)
#le bagging
set.seed(123)
randomForest(remontada ~ .,
             data = train, 
             mtry = 4,
             ntree = 500,
             importance = TRUE,
             keep.forest = TRUE) -> atp_bagging
predict(atp_bagging, newdata = test) -> yhat
# Confusion matrix for bagging
table(test$remontada, yhat) -> conf_mat
tx_err_bagging <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_bagging

# erreur = 0.498 ==> pire 

atp_bagging$confusion

#sur le train il donne des resultats mieux que le bagging sur tte les variables

# Importance des variables
atp_bagging$importance
varImpPlot(atp_bagging)



#################"boosting #######################

library('gbm')
set.seed(123)
# Conversion adequation en variable binaire

atp_cluster_test$remontada=as.numeric(atp_cluster_test$remontada)-1
train=atp_cluster_test[index,]
test=atp_cluster_test[-index,]
# Boosting classification tree with gbm
set.seed(123)
atp_boost <- gbm(remontada ~ .,
                 data = train,
                 distribution = "bernoulli",
                 n.trees = 500, 
                 interaction.depth = 4, 
                 shrinkage = 0.01) #shrinkage = learning rate (hyper-parameter to be set by user)
as.numeric(predict(atp_boost, newdata = test, n.trees = 500,
                   type = "response") > 0.5) -> yhat
# Confusion matrix for boosting
table(test$remontada, yhat) -> conf_mat
tx_err_boost <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_boost

# le boosting donne un taux d'erreur de 0.44

################adaboost

X_train=as.matrix(train[,-20])
y_train=(as.matrix(train[,20])*2) -1
X_test=as.matrix(test[,-20])
y_test=(as.matrix(test[,20])*2) -1

library(JOUSBoost)
adaboost(X_train, y_train, tree_depth = 1, n_rounds = 500)->atp_adaboost

predict(atp_adaboost,X_test)->yhat


table(y_test, yhat) -> conf_mat
tx_err_ada_boost <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_ada_boost

# adaboost donne un taux d erreur de 0.47

########################## Pour l'echontillonnage issu du clustering###

######Bagging 
library(randomForest)
Echantillon_atp$remontada=as.factor(Echantillon_atp$remontada)
#train test split
set.seed(123)
index=sample(1:nrow(Echantillon_atp),0.7*nrow(Echantillon_atp),replace = FALSE)
train=Echantillon_atp[index,]
test=Echantillon_atp[-index,]
dim(train); dim(test)

#verification de l'equilibre au niveau de la variable cible 
train%>%group_by(remontada)%>%summarise(N=n())

#le bagging
set.seed(123)
randomForest(remontada ~ .,
             data = train, 
             mtry = 19,
             ntree = 500,
             maxnodes=20,
             importance = TRUE,
             keep.forest = TRUE) -> atp_bagging
predict(atp_bagging, newdata = test) -> yhat
# Confusion matrix for bagging
table(test$remontada, yhat) -> conf_mat
tx_err_bagging <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_bagging

# erreur du bagging est de 0.43 mieux que le boosting,bagging, adaboost sur l'echantionnage aleatoire
## Lecture de quelques résultats
# Matrice de confusion sur données train
atp_bagging$confusion

#ici le bagging classifie correctement les joueurs qui font remontada dans 73% des cas sur le train, c enorme !! 
# par contre sur l'autre classe il fait n'importe koi 

# Importance des variables
atp_bagging$importance
varImpPlot(atp_bagging)




set.seed(123)
index=sample(1:nrow(Echantillon_atp),0.7*nrow(Echantillon_atp),replace = FALSE)
train=Echantillon_atp[index,c("minutes","w_svpt","l_svpt","w_2ndWon","w_1stIn","remontada")]
test=Echantillon_atp[-index,c("minutes","w_svpt","l_svpt","w_2ndWon","w_1stIn","remontada")]
dim(train); dim(test)



#le bagging
set.seed(123)
randomForest(remontada ~ .,
             data = train, 
             mtry = 5,
             ntree = 500,
             importance = TRUE,
             keep.forest = TRUE) -> atp_bagging
predict(atp_bagging, newdata = test) -> yhat
# Confusion matrix for bagging
table(test$remontada, yhat) -> conf_mat
tx_err_bagging <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_bagging

# c pire que le bagging precedent qui est sur tte les variables

## Lecture de quelques résultats
# Matrice de confusion sur données OOB
atp_bagging$confusion
# Importance des variables
atp_bagging$importance
varImpPlot(atp_bagging)



#################"boosting #######################

library('gbm')
set.seed(123)
# Conversion adequation en variable binaire

Echantillon_atp$remontada=as.numeric(Echantillon_atp$remontada)-1
train=Echantillon_atp[index,]
test=Echantillon_atp[-index,]
# Boosting classification tree with gbm
set.seed(123)
atp_boost <- gbm(remontada ~ .,
                 data = train,
                 distribution = "bernoulli",
                 n.trees = 500, 
                 interaction.depth = 4, 
                 shrinkage = 0.001) #shrinkage = learning rate (hyper-parameter to be set by user)
as.numeric(predict(atp_boost, newdata = test, n.trees = 500,
                   type = "response") > 0.5) -> yhat
# Confusion matrix for boosting
table(test$remontada, yhat) -> conf_mat
tx_err_boost <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_boost

# toujours le meilleure résultat est celuis du bagging avc tte les vars

################adaboost

X_train=as.matrix(train[,-20])
y_train=(as.matrix(train[,20])*2) -1
X_test=as.matrix(test[,-20])
y_test=(as.matrix(test[,20])*2) -1

library(JOUSBoost)
adaboost(X_train, y_train, tree_depth = 2, n_rounds = 500)->atp_adaboost

predict(atp_adaboost,X_test)->yhat


table(y_test, yhat) -> conf_mat
tx_err_ada_boost <-(conf_mat[1,2] + conf_mat[2,1]) / sum(conf_mat)
tx_err_ada_boost

#le meilleur est tjrs le bagging 



#Conclusion : comme le bagging qui est sur tte les variables donne les meilleurs resultats 
# sur le test, donc on l'applique sur la totalité de notre echantillon et retient les variables qui expliquent plus notre variable cible



Echantillon_atp$remontada=as.factor(Echantillon_atp$remontada)
set.seed(123)
randomForest(remontada ~ .,
             data = Echantillon_atp, 
             mtry = 19,
             ntree = 500,
             maxnodes=20,
             importance = TRUE,
             keep.forest = TRUE) -> atp_bagging


# Matrice de confusion sur les données 
atp_bagging$confusion

#ici le bagging classifie correctement les joueurs qui font remontada dans 63% des cas sur le train, c déja bien !! 
# par contre sur l'autre classe il fait n'importe koi c comme du pile ou face

# Importance des variables
atp_bagging$importance
varImpPlot(atp_bagging)


#on peut donc retenir les variables suivantes comme meileures en termes d'explication
# W_svpt, l_svpt, w_2ndWon, minutes
