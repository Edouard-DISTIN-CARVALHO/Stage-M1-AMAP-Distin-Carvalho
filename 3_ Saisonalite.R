#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#       3/ Etude de la saisonalité de la productivité primaire                  ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/1_Normalisation.R") 
source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/2_Composition.R") 

library(lubridate)
library(dplyr)
library(ade4)
library(lme4)
library(ggplot2)

##### Différence de jour entre les pics de chaque année #####

# Convertir la variable date en format de date
dados_norm$date <- as.Date(dados_norm$date)

# Groupement par année et recherche des dates des pics de "leaves" et "twigs"
peaks <- dados_norm %>%
  group_by(fire_regime, year = lubridate::year(date)) %>%
  summarise(date_max_leaves = date[which.max(leaves)],
            date_max_twigs = date[which.max(twigs)])

# Calculer le nombre de jours entre les dates des pics de "leaves" et "twigs"
peaks <- peaks %>% mutate(days_between_leaves = as.integer(date_max_leaves - lag(date_max_leaves)))
peaks <- mutate(peaks, days_between_leaves = ifelse(is.na(days_between_leaves), 0, days_between_leaves))
peaks <- peaks %>% mutate(days_between_twigs = as.integer(date_max_twigs - lag(date_max_twigs)))
peaks <- mutate(peaks, days_between_twigs = ifelse(is.na(days_between_twigs), 0, days_between_twigs))

ggplot(peaks, aes(x = year, y = days_between_leaves, shape = fire_regime, color = fire_regime, linetype = fire_regime)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = fire_regime)) + 
  labs(title = "Droite de régression des différences de pic de productivité primaire maximale des feuilles",
       x = "Année",
       y = "Nombre de jours entre les pics",
       color = "Fire Regime") +
  scale_shape_manual(values = c(19,15,17,1,22,24)) +  
  scale_color_manual(values = color) +  
  scale_linetype_manual(values = linetype) +
  theme_classic()

ggplot(peaks, aes(x = year, y = days_between_twigs, shape = fire_regime, color = fire_regime, linetype = fire_regime)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = fire_regime)) + 
  labs(title = "Droite de régression des différences de pic de productivité primaire maximale des brindilles",
       x = "Année",
       y = "Nombre de jours entre les pics",
       color = "Fire Regime") +
  scale_shape_manual(values = c(19,15,17,1,22,24)) +  
  scale_color_manual(values = color) +  
  scale_linetype_manual(values = linetype) +
  theme_classic()
