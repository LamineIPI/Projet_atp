#Importation de fichiers

# Packages
library(tidyverse)
library("stringr")
library('purrr')
### Importation de la atp_players contenant les identifiants des joueurs
players <- read_csv(file = "data/atp_players.csv",
                    col_names = FALSE)
names(players) <- c("id", "firstname", "lastname", "hand", "birthday", "nat")


### Importation des fichiers des matchs atp joués de 1968 à 2020
lst <- list.files(path = "data/")
lst_data <- grep(pattern = "^atp_matches_[[:digit:]]{4}.csv$", x = lst, value = TRUE)
lst_names <- paste('atp', str_extract(string = lst_data, pattern = "[[:digit:]]{4}"), sep = "")
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("data/", x, sep = "")))
names(lst_tib) <- lst_names

atp <- reduce(.x = lst_tib, .f = bind_rows)
head(atp)

# Ajout de la variable years 
atp <- atp %>% 
  mutate(years = str_extract(string = tourney_id, pattern = "[[:digit:]]{4}"))

# Modification  de la variable surface
atp %>%
  mutate(surface = factor(surface, levels = names(sort(table(surface))), ordered = TRUE)) %>%
  #Toilettage en prévision de l'inclusion dans un rapport ou un graphique
  rename(Surface = surface) %>%
  mutate(Surface = case_when(Surface == "Clay" ~ "Terre battue",
                             Surface == "Grass" ~ "Herbe",
                             Surface == "Hard" ~ "Dur")) -> atp

