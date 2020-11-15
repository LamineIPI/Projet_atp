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

