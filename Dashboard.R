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

#GIT ANAS