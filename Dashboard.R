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

<<<<<<< HEAD
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


