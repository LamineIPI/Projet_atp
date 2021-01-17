#Choix du joueur :

J_Prenom <- "Rafael"
J_Nom <- "Nadal"

# Choix de la saison :

annee = "2013"

#Importation de fichiers

# Packages
library(tidyverse)
library("stringr")
library('purrr')
library('rmarkdown')
library('flexdashboard')
library("ggthemes")
library('DBI')
library('fmsb')
library('forcats')
library("lubridate")
library('tidyr')
library(rvest)
library(magick)
require(tidyverse)
library(data.table)
library(knitr)

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

## Focus sur le joueur

## Copie de l'année 
atp_matches_2013 <- atp  %>% # la variable years
  filter(years ==annee)

# L'identifiant du joueur
players %>% 
  filter(firstname == J_Prenom & lastname == J_Nom) %>%
  select(id) %>% 
  as.numeric() -> id_Nadal

# Filtrer sur les matchs du joueur
atp_Nadal <- atp_matches_2013 %>%
  filter(winner_id==id_Nadal | loser_id == id_Nadal)
               
# Conversion de la variable birthday en date et exctraction de l'ann?e de naissance
players$birthday<- as.Date(as.character(players$birthday), format = "%Y%m%d")
players %>% 
  filter(firstname == J_Prenom & lastname == J_Nom) %>%
  select(birthday) -> J_Naissance
J_age <- as.data.frame(J_Naissance)
J_age <- year(J_age[1,1])
J_age <- as.numeric(annee)-J_age
               
# Importation de la base classement -------------------------------------------------------------------------
lst <- list.files(path = "data/")
lst_data <- grep(pattern = "^atp_rankings_[[:digit:]]{2}s.csv$", x = lst, value = TRUE)
lst_names <- paste('atp_rankings_', str_extract(string = lst_data, pattern = "[[:digit:]]{2}"), sep = "") 
lst_tib <- map(.x = lst_data, function (x) read_csv(paste("data/", x, sep = "")))
names(lst_tib) <- lst_names
atp_classement <- reduce(.x = lst_tib, .f = bind_rows)

#Importation du classement actuel
atp_rankings_current <- read_csv("data/atp_rankings_current.csv",
                                 col_names = FALSE, locale = locale(date_names = "fr"))
colnames(atp_rankings_current) <- colnames(atp_classement)

atp_classement %>%
  bind_rows(atp_rankings_current) %>%
  mutate(ranking_date = ymd(ranking_date))->atp_classement

render("dash.rmd")
