#Diagramme de Kiviat Nadal
library('DBI')
library(tidyverse)
library(readxl)
library(tidyr)
library(fmsb)


####################################################################   Nb service  ########################################################################
####On aurait pu le faire plus simplement avec ifelse(atp_Nadal$winner_id=='104745',atp_Nadal$w_svpt,atp_Nadal$l_svpt) (a adapter a toute les variables)
########################################
#### nb service quand il win
########################################
atp_Nadal %>%
  filter(winner_id =='104745')
summary(atp_Nadal$w_svpt)
library('DBI')
sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)

requete4 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT w_svpt FROM atp_Nadal WHERE winner_id = '104745'")
requete4
tab_w_svpt<- dbFetch(requete4)

summary(tab_w_svpt)


########################################
# nb service il lose
########################################


atp_Nadal %>%
  filter(loser_id =='104745')

sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)

requete5 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT l_svpt FROM atp_Nadal WHERE loser_id = '104745'")
requete5
tab_l_svpt<- dbFetch(requete5)
summary(tab_l_svpt)

#concaténer win+lose 
colnames(tab_l_svpt) <- "w_svpt" #Rend possible la concaténation
Serve_points <-rbind(tab_w_svpt,tab_l_svpt)
summary(Serve_points)
#Faire la somme de tout les services
serv_tot <- colSums(Serve_points,na.rm = T)

####################################################################   ACE   ########################################################################
########################################
#### Ace raf quand il win
########################################
atp_Nadal %>%
  filter(winner_id =='104745')
summary(atp_Nadal$w_ace)

sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)

requete <- dbSendQuery(conn = sqlite_con, statement
                       = "SELECT w_ace FROM atp_Nadal WHERE winner_id = '104745'")
requete  
tab_ace<- dbFetch(requete)
summary(tab_ace)



########################################
#### Ace raf quand il lose
########################################
atp_Nadal %>%
  filter(loser_id =='104745')
summary(atp_Nadal$l_ace)

sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé?
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)
requete2 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT l_ace FROM atp_Nadal WHERE loser_id = '104745'")
requete2  
tab_lace<- dbFetch(requete2)
summary(tab_lace)
#concaténer win+lose 
colnames(tab_lace) <- "w_ace" #Rend possible la concaténation
Ace <-rbind(tab_ace,tab_lace)
summary(Ace)
ace_tot <- colSums(Ace,na.rm = T)

####################################################################   (1stWon) point remporter premier serveur  ########################################################################
########################################
#### 1stWon raf quand il win
########################################
atp_Nadal %>%
  filter(winner_id =='104745')
summary(atp_Nadal$w_1stWon)

sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)

requete3 <- dbSendQuery(conn = sqlite_con, statement
                       = "SELECT w_1stWon FROM atp_Nadal WHERE winner_id = '104745'")
requete3  
tab_w_1stWon<- dbFetch(requete3)
summary(tab_w_1stWon)


########################################
#### 1stWon raf quand il lose (le match)
########################################
atp_Nadal %>%
  filter(loser_id =='104745')
summary(atp_Nadal$l_1stWon)

sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On sp?cifie le pilote utilis?
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)
requete4 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT l_1stWon FROM atp_Nadal WHERE loser_id = '104745'")
requete4  
tab_l_1stWon<- dbFetch(requete4)
summary(tab_l_1stWon)

#concaténer win+lose 
colnames(tab_l_1stWon) <- "w_1stWon" #Rend possible la concaténation
fstWon <-rbind(tab_w_1stWon,tab_l_1stWon)
summary(fstWon)
fstWon_tot <- colSums(fstWon,na.rm = T)

####################################################################   (2ndWon) point remporter second service  ########################################################################
########################################
#### 2ndWon raf quand il win
########################################
atp_Nadal %>%
  filter(winner_id =='104745')
summary(atp_Nadal$w_2ndWon)

sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)

requete5 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT w_2ndWon FROM atp_Nadal WHERE winner_id = '104745'")
requete5  
tab_w_2ndWon<- dbFetch(requete5)
summary(tab_w_2ndWon)


########################################
#### 2ndWon raf quand il lose (le match)
########################################
atp_Nadal %>%
  filter(loser_id =='104745')
summary(atp_Nadal$l_2ndWon)

sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)
requete6 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT l_2ndWon FROM atp_Nadal WHERE loser_id = '104745'")
requete6  
tab_l_2ndWon<- dbFetch(requete6)
summary(tab_l_2ndWon)

#concaténer win+lose 
colnames(tab_l_2ndWon) <- "w_2ndWon" #Rend possible la concaténation
sndWon <-rbind(tab_w_2ndWon,tab_l_2ndWon)
summary(sndWon)
sndWon_tot <- colSums(sndWon,na.rm = T)



####################################################################   Nb break points faced  ########################################################################

########################################
#### Nb break points faced quand il win
########################################
atp_Nadal %>%
  filter(winner_id =='104745')
summary(atp_Nadal$w_bpFaced)
library('DBI')
sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)

requete7 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT w_bpFaced FROM atp_Nadal WHERE winner_id = '104745'")
requete7
tab_w_bpFaced<- dbFetch(requete7)

summary(tab_w_bpFaced)


########################################
# Nb break points faced il lose
########################################
atp_Nadal %>%
  filter(loser_id =='104745')

sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)

requete8 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT l_bpFaced FROM atp_Nadal WHERE loser_id = '104745'")
requete8
tab_l_bpFaced<- dbFetch(requete8)
summary(tab_l_bpFaced)

#concaténer win+lose 
colnames(tab_l_bpFaced) <- "w_bpFaced" #Rend possible la concaténation
Break_points_faced <-rbind(tab_w_bpFaced,tab_l_bpFaced)
summary(Break_points_faced)
#Faire la somme de tout les break point "faced"
w_bpFaced_tot <- colSums(Break_points_faced,na.rm = T)

####################################################################   Nb break points saved  ########################################################################

########################################
#### Nb break points saved quand il win
########################################
atp_Nadal %>%
  filter(winner_id =='104745')
summary(atp_Nadal$w_bpSaved)
library('DBI')
sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)

requete9 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT w_bpSaved FROM atp_Nadal WHERE winner_id = '104745'")
requete9
tab_w_bpSaved<- dbFetch(requete9)

summary(tab_w_bpSaved)


########################################
# Nb break points saved il lose
########################################
atp_Nadal %>%
  filter(loser_id =='104745')

sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilisé
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)

requete10 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT l_bpSaved FROM atp_Nadal WHERE loser_id = '104745'")
requete10
tab_l_bpSaved<- dbFetch(requete10)
summary(tab_l_bpSaved)

#concaténer win+lose 
colnames(tab_l_bpSaved) <- "w_bpSaved" #Rend possible la concaténation
Break_points_Saved <-rbind(tab_w_bpSaved,tab_l_bpSaved)
summary(Break_points_Saved)
#Faire la somme de tout les break point "Saved"
w_bpSaved_tot <- colSums(Break_points_Saved,na.rm = T)




########################################
#Moyenne des service gagnant pour chaque joueur en 2013 (MAX) (useless pour l'instant)
########################################
sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On spécifie le pilote utilis?
                       dbname = ":memory:")
dbWriteTable(conn = sqlite_con, name = "atp2013", value = atp2013)

requete6 <- dbSendQuery(conn = sqlite_con, statement
                        = "SELECT winner_name, avg(w_svpt) FROM atp2013 group by winner_name order by avg(w_svpt)")
requete6  
tab_avg_servepoint_2013<- dbFetch(requete6)
summary(tab_avg_servepoint_2013)

#################################################################### Mise en forme diagramme ########################################################################
##Création de la base pour la diagramme

Raf_radar <- data.frame(
  "Break Points Sauvés" = c(100,0,(w_bpSaved_tot/w_bpFaced_tot)*100),
  "Ace" = c(10,0,(ace_tot/serv_tot)*100),
  "Deuxieme Service Gagnant" = c(100,0,(sndWon_tot/serv_tot)*100),
  "Premier Service Gagnant" = c(100,0,(fstWon_tot/serv_tot)*100)
  
)

colnames(Raf_radar)<-c("Break Points Sauvés","Aces*","Deuxiemes Services Gagnants","Premiers Services Gagnants" ) #Contre l'affichage des noms avec des points

#View(Raf_radar)
radarchart(Raf_radar,axistype=1 ,
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
           
           #custom labels
           vlcex=0.8 
)
title("Statistiques récapitulatives de Nadal (en %)")
text(-0.15, -0.07, labels = "0 (%)", col="#999966")
text(-1, -0.07, labels = "10 (%)", col="#999966")
text(-0.55, -0.07, labels = "5 (%)", col="#999966")

dbDisconnect(conn = sqlite_con)
