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


