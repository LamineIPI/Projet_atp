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

ff
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

#Type de sol
table(atp_Nadal$Surface)
Pourcentage=data.frame(group=c("terre battue","herbe","dur"),value=c(49.4,1.2,49.4))
ggplot(Pourcentage,aes(x="",y=value,fill=group)) + 
  geom_bar(width = 1,stat="identity")  + 
  coord_polar("y",start=0) +
  labs(caption = "Source : Les auteurs") +
  scale_fill_discrete(type = c('terre battue' = 'chocolate', 'herbe' = 'chartreuse4', 'dur' = "deepskyblue4")) + 
  theme_void()  + 
  geom_text(aes(label = paste(as.numeric(value/100)*100,"%")),color="black", size=5 ,
            position = position_stack(vjust = 0.5)) +
  ggtitle("R?partition de tous les matchs en fonction de leur surface") +
  theme(plot.title = element_text(hjust = 0.5))

## diagramme Surface + best of 
atp_Nadal %>%
  filter(Surface=="Terre battue" | Surface =="Dur") %>%
  count(Surface=factor(Surface), best_of= factor(best_of)) %>% 
  ungroup() %>% 
  mutate(pct=n/sum(n) *100) %>%
  ggplot(aes(x=Surface,y=pct,fill=best_of)) + 
  geom_bar(stat = 'identity', position = "fill") +
  geom_text(aes(y = pct, label = c("80.5%","19.5%","82.9%","17.1%")),  
            position = "fill", vjust=3,
            size = 4) +
  ggtitle("Le nombre de best of effectué par Raphael Nadal en fonction de la surface du terrain") +
  scale_fill_manual(values = c("royalblue", "lightblue")) + 
  theme_minimal()+
  labs(caption = "Source : Les auteurs") +
  xlab("Surface") + ylab("best of") +
  theme(plot.title = element_text(hjust = 0.5))

## diagramme victoire + Bestof dans un match

ggplot(atp_Nadal,aes(x=Raph_vic, fill=as.character(best_of))) +
  geom_bar(position = "dodge") + 
  scale_fill_manual(values= c( "royalblue","lightblue"))+
  geom_text(aes(label=after_stat(count)),stat = "count",
            position = position_dodge(0.9)) +
  ggtitle("Le nombre de set par match en fonction de  \n la victoire ou d?faite de Raphael Nadal") + 
  labs(caption = "Source : Les auteurs") +
  xlab("redoublement") + ylab("Effectifs") +
  theme( plot.title = element_text(hjust = 0.5) ) + 
  theme_minimal()

## diagramme victoire + Main dominante de l'adversaire
main=ifelse(atp_Nadal$winner_id=='104745',atp_Nadal$loser_hand,atp_Nadal$winner_hand)

ggplot(atp_Nadal,aes(x=Raph_vic, fill=main)) +
  geom_bar(position = "dodge") + 
  scale_fill_manual(values= c( "royalblue","lightblue"))+
  geom_text(aes(label=after_stat(count)),stat = "count",
            position = position_dodge(0.9)) +
  ggtitle("La main dominante de son adversaire en fonction de  \n la victoire ou d?faite de Raphael Nadal") + 
  labs(caption = "Source : Les auteurs") +
  xlab("redoublement") + ylab("Effectifs") +
  theme( plot.title = element_text(hjust = 0.5) ) + 
  theme_minimal()
