## Focus sur Rafael Nadal

## Copie de l'année 2013
atp_matches_2013 <- atp  %>% # la variable years
  filter(years =="2013")

# L'identifiant de Nadal
players %>% 
  filter(firstname == 'Rafael' & lastname == 'Nadal') %>%
  select(id) %>% 
  as.numeric() -> id_Nadal

# Filtrer par Nadal
atp_Nadal <- atp_matches_2013 %>%
  filter(winner_id==id_Nadal | loser_id == id_Nadal) 

# création d'une colonne pour savoir lorsque nadal gagne ou perd 
for(i in 1:nrow(atp_Nadal)){
  if( atp_Nadal[i,8] == 104745){
    atp_Nadal[i,50]="win"
  }else{
    atp_Nadal[i,50]="loose"
  }}

names(atp_Nadal)[50] ="Raph_vic"


#Nombre de match par tournoi
atp_Nadal %>%
  group_by(tourney_name)%>%
  summarize(`Nombre de matchs` = n())

# Victoires et défaites par saison
atp_Nadal %>%
  mutate(resultat = ifelse(winner_id==id_Nadal, 'win', 'lost')) %>%
  group_by(resultat)%>%
  summarize(`matchs gagnés` = n())

# Matchs joués selon la surface
atp_Nadal %>%
  group_by(Surface) %>%
  summarize(`matchs joués` = n())

# Nombre de titre par saison
atp_Nadal%>%
  mutate(resultat = ifelse(winner_id==id_Nadal, 'win', 'lost'))%>%
  filter(round == "F", resultat == "win")%>%
  summarise(Titres = n())

#Moyenne des minutes par match :
summary(atp_Nadal$minutes)
#Moyenne de 108.5

#Nombre de match disputé en bo3 ou bo5
table(atp_Nadal$best_of)
#67 bo3 pour 16 bo5

#Ace gagnant par Raphael Nadal
summary(atp_Nadal$w_ace)
atp_Nadal %>%
  filter(winner_id =='104745')
  summary(atp_Nadal$w_ace)
  library('DBI')
  sqlite_con <-dbConnect(drv = RSQLite::SQLite(), #On sp?cifie le pilote utilis?
                         dbname = ":memory:")
  dbWriteTable(conn = sqlite_con, name = "atp_Nadal", value = atp_Nadal)
  
requete <- dbSendQuery(conn = sqlite_con, statement
                       = "SELECT w_ace FROM atp_Nadal WHERE winner_id = '104745'")
requete  
tab_ace<- dbFetch(requete)
summary(tab_ace)
#En moyenne Rafael nadal fait 2.77 Ace dans un match o? il a gagn?
# Il a fait au maximum 9 Ace dans un seul match

requete1 <- dbSendQuery(conn = sqlite_con, statement
                       = "SELECT l_ace FROM atp_Nadal WHERE loser_id = '104745'")
requete1  
tab_lace<- dbFetch(requete1)
colnames(tab_lace) <- "w_ace"

Ace <-rbind(tab_ace,tab_lace)

summary(Ace)
#En moyenne Rafael nadal a fait 2.728 Ace par match lors de sa saison
# Il a fait au maximum 9 Ace dans un seul match

