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



