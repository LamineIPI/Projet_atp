
# Projet_atp Apprentissage Automatique. 
Ce projet a pour but d'atteindre l'objectif suivant : 
- L'identification des facteurs permettant d’expliquer les retournements de situation (gagner un match en 5 sets en étant mené 2 sets à 0)

Vous pourrez retrouver le fichier pdf :"Remontada_rapport.pdf" présentant la méthodologie abordée durant ce projet ainsi que les résultats obtenus.

# Table des matières du rapport

1 Introduction

2 Sélection aléatoire

2.1 Arbre de Classification

2.2 La méthode Bagging 

2.3 Etude de la Random Forest 

2.4 Méthode de Boosting

2.5 Régression logistique

3 Sélection par échantillonnage des données

3.1 La méthode Bagging 

3.2 Etude de la Random Forest 

3.3 Boosting 

3.4 Régression logistique 

4 Conclusion

A Annexes

# Résumé du travail effectué

Nous avons donc choisi d’étudier uniquement les matchs qui se sont déroulés
en 5 sets. Nous avons décidé de réaliser cette mission de plusieurs façons. Dans un premier temps, nous
avons travaillé sur une base de données constituée des 421 matchs correspondant à une remontée et de 421
matchs pris aléatoirement dans les matchs normaux en 5 sets. Dans un second temps nous avons réalisé
un partitionnement sur les 2182 observations des matchs sans remontada pour calculer la proportion des
observations dans chaque cluster. Chaque cluster va être représenté dans les mêmes proportions parmi les
421 observations. Ainsi, nous pourrons comparer les résultats obtenus grâce aux différentes méthodes vues en cours
pour les deux bases de données.


# Exécution du programme à partir d'un terminal
(Creez un dossier nommé "data" contenant toutes les bases de données atp.)

  Ouvrir un terminal et taper les lignes suivantes :
 - git clone https://github.com/LamineIPI/Projet_atp.git
 - cd Projet_atp
 - R CMD BATCH CLUSTER_FIN.R
 
 Un fichier Remontada_rapport.pdf est ensuite créé dans le dossier Projet_atp.
