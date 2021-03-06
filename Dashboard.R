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

################################### Tournois disputés et résultats obtenus ############################################
atp_Nadal %>%
  mutate(result =case_when(winner_id == id_Nadal ~ "Won",
                           winner_id != id_Nadal ~ 'Lost')) %>%
  select(tourney_id,  tourney_date, tourney_name, tourney_level, Surface, round, result) %>%
  mutate(tourney_result = case_when(round == 'F' & result == 'Won' ~ 'F gagnés', #F gagnés en cas de victoire finale
                                    result == 'Lost' ~ paste0(round," ", "perdus") )) %>% # Le rang du tournoi de la défaite sinon
  filter(!is.na(tourney_result)) %>%
  select(-round, -result) %>%
  distinct() %>%
  arrange(tourney_date)-> tour_Nadal

tour_Nadal%>%
  group_by(tourney_result)%>%
  summarise(N = n())%>%
  ggplot(mapping = aes(x=tourney_result, fill = tourney_result, y = N))+
  geom_col()+
  scale_fill_manual(values = c("chartreuse4", "chocolate",  "#A7B800", "#E8B800"))+
  theme_bw()+
  geom_text(mapping = aes(y = N, label = N)) +
  labs(y = "Nombre de tournoi", x = 'Résultat du obtenu') +
  labs(fill = "Résultats obtenus")

######################################## Adversaires rencontrés et resultats des confrontations ##########################
library(forcats)
atp_Nadal %>%
  mutate(resultat = ifelse(winner_id==id_Nadal, 'won', 'lost'),
         Adversaire = ifelse(winner_id==id_Nadal, loser_name, winner_name)) %>%
  select(winner_name, resultat, Adversaire) %>%
  group_by(Adversaire, resultat) %>%
  summarize(Nb_matchs = n())  %>%
  ggplot(mapping = aes(x = fct_reorder(Adversaire, Nb_matchs, .fun=sum), fill= resultat, y = Nb_matchs)) +
  geom_col(position = "stack", width = .9) +
  coord_flip() +
  scale_fill_discrete(type = c("won" = "springgreen3", 'lost' = "tomato3"))+
  ylab("Nombre de confrontations") +
  xlab("Adversaires") +
  theme_bw() +
  labs(fill = "Résultat")



#creation de nouvelles variables 
atp_Nadal%>%mutate(Nadal_win=ifelse(winner_name=='Rafael Nadal',1,0))->D
D%>%mutate(bpsaved=ifelse(Nadal_win==1,w_bpSaved,l_bpSaved))->D
D%>%mutate(ace=ifelse(Nadal_win==1,w_ace,l_ace))->D






#surface et ace  je dois penser a faire la freq
library("ggthemes")
D%>%group_by(Surface)%>%summarise(Nb_ace=sum(ace,na.rm = TRUE))%>%mutate(freq=Nb_ace/sum(Nb_ace),lab=paste0(round(freq*100,0),"%"))->aces

ggplot(aces)+geom_bar(aes(x=Surface,y=freq,fill=Surface),stat='identity')+xlab('Surface')+ylab('% d ace marqués')+
  ggtitle('Ace marqués en fonction de la surface du terrain')+
  geom_text(mapping = aes(x=Surface,y = freq, label = lab)) +
  scale_fill_manual(values=c("#4E84C4","#52854C","#D16103"))+
  theme_economist()+
  theme(legend.position="right")

#surface et BALLE DE BREAK sauvé 
D%>%group_by(Surface)%>%summarise(Nb_bpsaved=sum(bpsaved,na.rm = TRUE))%>%mutate(freq=Nb_bpsaved/sum(Nb_bpsaved),lab=paste0(round(freq*100,0),"%"))->bpsaveds


ggplot(bpsaveds)+geom_bar(aes(x=Surface,y=freq,fill=Surface),stat='identity')+xlab('Surface')+ylab('% de balles de break sauvées')+
  ggtitle('Balles de break sauvées en fonction de la surface du terrain')+
  geom_text(mapping = aes(x=Surface,y = freq, label = lab)) +
  scale_fill_manual(values=c("#4E84C4","#52854C","#D16103"))+
  theme_economist()+
  theme(legend.position="right")




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
  ggtitle("Répartition de tous les matchs en fonction de leur surface") +
  theme(plot.title = element_text(hjust = 0.5))

## diagramme Surface + best of 
atp_Nadal %>%
  filter(Surface=="Terre battue" | Surface =="Dur") %>%
  count(Surface=factor(Surface), best_of= factor(best_of)) %>% 
  count(Surface=factor(surface), best_of= factor(best_of)) %>%
  ungroup() %>% 
  mutate(pct=n/sum(n) *100) %>%
  ggplot(aes(x=Surface,y=pct,fill=best_of)) + 
  geom_bar(stat = 'identity', position = "fill") +
  geom_text(aes(y = pct, label = c("80.5%","19.5%","82.9%","17.1%")),  
            position = "fill", vjust=3,
            size = 4) +
  ggtitle("Le nombre de best of effectué par Raphael Nadal en fonction de la surface du terrain") +
  ggtitle("Le nombre de best of effectu? par Raphael Nadal en fonction de la surface du terrain") +
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

#################################################Nadal vs Top_10#########################################################

library(fmsb)

#Nombre de match gagné des top10 par surface 
atp_matches_2013 %>%
  filter((winner_name != "Rafael Nadal" & winner_rank < 11) | (loser_name != "Rafael Nadal" & loser_rank < 11)) %>%
  group_by(Surface) %>%
  summarize(nombre_victoire = n()) -> nbr_game_top10

#Création de la variable "nombre de match joué" pour calculer le taux des gains pour les top10 
atp_matches_2013 %>%
  filter(winner_name != "Rafael Nadal" & winner_rank < 11) %>%
  group_by(Surface) %>%
  summarize(nombre_victoire = n()) %>%
  mutate(nb_match =nbr_game_top10$nombre_victoire, taux = round(nombre_victoire/nb_match,2)) %>%
  as.data.frame() -> nbr_game_top10_win 

#Multiplier le taux des gains des top10 par le nombre de match joué par nadal 
#afin de comparer nadal et les top10
nbr_game_top10_win <- nbr_game_top10_win %>%
  mutate(nb_gameNadal = c(41,1,41),victoire_top10=nb_gameNadal*taux)

#Nombre de match gagné de Rafael Nadal par surface
atp_Nadal %>% 
  filter(winner_name == "Rafael Nadal") %>%
  group_by(Surface) %>%
  summarize(Nombre_match =n()) -> Kiviat_Nb

#Mise en forme du diagramme 
Kiviat_Nb <- rbind(Kiviat_Nb,c("Herbe",0))
min <- rep(0,each=3)
max <- c(41,41,1)
tible <- t(cbind(max,min,Kiviat_Nb))[-3,]
tible <- as.data.frame(tible,)
tible <- rbind(tible, Nombre_match_top10_win = nbr_game_top10_win[c(1,3,2),6])
colnames(tible) <- c("victoire sur \nterre battue","victoire sur terre \ndure","victoire \nsur gazon")
for(i in 1:3){
  tible[,i] <- as.numeric(tible[,i])
}

radarchart(tible,axistype=1,pcol=c("red","black"), plwd=3 , cglcol=c("grey"), cglty=1, 
           axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.6,vlcex=0.8, title = "Nadal vs Top10") 

legend(-2,1.25, legend=c("Resultats Nadal", "Resultats top10"),
       col=c("red", "black"),lwd = 3 ,lty=1:2, cex=0.6,bty="n")
