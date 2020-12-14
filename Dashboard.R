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

#creation de nouvelles variables 
atp_Nadal%>%mutate(Nadal_win=ifelse(winner_name=='Rafael Nadal',1,0))->D
D%>%mutate(df=ifelse(Nadal_win==1,w_df,l_df))->D
D%>%mutate(BBS=ifelse(Nadal_win==1,w_bpSaved,l_bpSaved))->D
D%>%mutate(ace=ifelse(Nadal_win==1,w_ace,l_ace))->D
D%>%mutate(frsts=ifelse(Nadal_win==1,w_1stIn,l_1stIn))->D
D%>%mutate(fg=ifelse(Nadal_win==1,w_1stWon,l_1stWon))->D
D%>%mutate(sg=ifelse(Nadal_win==1,w_2ndWon,l_2ndWon))->D
D%>%mutate(gsg=ifelse(Nadal_win==1,w_SvGms,l_SvGms))->D
D%>%mutate(NB_S=ifelse(Nadal_win==1,w_svpt,l_svpt))->D




D$Nadal_win=as.factor(D$Nadal_win)
D$best_of=as.factor(D$best_of)


#gain et level de tournoie 
ggplot(D) + geom_bar(aes(x=D$tourney_level,fill=D$Nadal_win),position = 'dodge')+xlab('le niveau du tournoie')+
  ylab('Nombre de match')+ #labs(fill = "Resultat du match")
  scale_fill_discrete(name = "Resultat du match", labels = c("Gain", "Perte"))+
  ggtitle('Resultat des matchs joués en fonction du niveau de tournoie')
ggplot(D) + geom_bar(aes(x=D$best_of,fill=D$Nadal_win),position = 'dodge')

#Gain ou perte et df 
m=as.data.frame(D%>%group_by(Nadal_win)%>%summarise(moyenne=mean(df,na.rm = TRUE)))
ggplot(m) + geom_bar(aes(y=m$moyenne,x=m$Nadal_win),stat = 'identity')

#Gain ou perte et BALLE DE BREAK sauvé 
BBSM=as.data.frame(D%>%group_by(Nadal_win)%>%summarise(moyenne=mean(BBS,na.rm = TRUE)))
ggplot(BBSM) + geom_bar(aes(y=BBSM$moyenne,x=BBSM$Nadal_win),stat = 'identity')

#perte et main du winner :
W_H=D%>%filter(Nadal_win==0)%>%select(winner_hand)
W_H=as.data.frame(W_H%>%group_by(winner_hand)%>%summarise(N=n(),prop=round(N/7,2)))
W_H<- W_H %>%
  arrange(desc(winner_hand)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
W_H

mycols <- c("#868686FF", "#CD534CFF")
#camambert
ggplot(W_H, aes(x = "", y = prop, fill = winner_hand)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop*100," %")), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()
#l autre diag 
ggplot(W_H, aes(x = 2, y = prop, fill = winner_hand)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label=paste(prop*100," %")), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)+
  scale_fill_discrete(name = "La main du gagnant", labels = c("Gaucher", "Droitier"))+
  ggtitle('Les joueurs qui ont gagné Nadal sont-ils droitiers ou gauchers')
#surface et round
SR=D%>%select(round,Surface)
ggplot(D) + geom_bar(aes(y=D$round,fill=D$Surface))+ xlab('Nombre de match')+
  ylab('Le niveau de match')+
  scale_fill_discrete(name = "Type de surface")+
  ggtitle('Le niveau de match disputé et la surface sur laquelle il est joué')

#frequence des droitiers ou gauchers qui ont perdu contre Nadal
D%>%filter(loser_name!="Rafael Nadal")%>%group_by(loser_hand)%>%summarise(somme=n()/76)

#frequence des droitiers ou gauchers dans tout les tournoie d'atp en 2013 sauf 
#Nadal (on prend pas en compte les U et NA)
atp_matches_2013%>%filter(winner_name!='Rafael Nadal'&loser_name!='Rafael Nadal')%>%group_by(winner_hand)%>%summarise(somm=n()/2842)

atp_matches_2013%>%filter(winner_name!='Rafael Nadal'&loser_name!='Rafael Nadal')%>%group_by(loser_hand)%>%summarise(somm=n()/2800)


#surface et ace  je dois penser a faire la freq
ggplot(D)+geom_bar(aes(x=D$Surface,y=D$ace,fill=Surface),stat='identity')+xlab('Surface')+ylab('Nombre d ace marqué')+
  scale_fill_discrete(name = "Type de surface")+
  ggtitle('Ace marqués en fonction de la surface du terrain')


ggplot(D)+geom_bar(aes(x=D$Surface,y=D$BBS,fill=Surface),stat='identity')+xlab('Surface')+ylab('Nombre de balle de break sauvée')+
  scale_fill_discrete(name = "Type de surface")+
  ggtitle('Nombre de balle de break sauvée en fonction de la surface du terrain')


#ace et round en fonction de win ou lose
ggplot(D)+geom_bar(aes(x=D$round,y=D$ace,fill=D$Nadal_win),position='dodge',stat = 'identity')+theme(
  panel.grid = element_blank(),
  panel.border = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "top"
) 

#kiviat concernant les services 

library(fmsb)

KV=D%>%select(ace,sg,fg,frsts,NB_S,gsg)


KV=na.omit(KV)

colMax <- function (x) { apply(x, MARGIN=c(2), max) }
colMin <- function (x) { apply(x, MARGIN=c(2), min) }
maxmin <- data.frame(max=colMax(KV),min=colMin(KV))

maxmin

average <- data.frame(rbind(maxmin$max,maxmin$min,t(colMeans(KV))))
colnames(average) <- c('ace gagnés','jeux de sevrce G','nombre de services','premiers services réussis','pnts G 1 er service','pnts G 2 éme service')

radarchart(average)


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
  count(Surface=factor(surface), best_of= factor(best_of)) %>% 
  ungroup() %>% 
  mutate(pct=n/sum(n) *100) %>%
  ggplot(aes(x=Surface,y=pct,fill=best_of)) + 
  geom_bar(stat = 'identity', position = "fill") +
  geom_text(aes(y = pct, label = c("80.5%","19.5%","82.9%","17.1%")),  
            position = "fill", vjust=3,
            size = 4) +
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
