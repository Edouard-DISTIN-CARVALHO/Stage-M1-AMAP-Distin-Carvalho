#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#       3/ Etude de la saisonalité de la productivité primaire                  ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getwd()
#setwd("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git") # AMAP
setwd("C:/Users/edoua/OneDrive/Documents/R/AMAP/Git") # Galaxybook
source("2_Composition.R") 

library(lubridate)
library(dplyr)
library(ggplot2)

### Différence d'amplitude moyenne entre les pics de chaque année pour chaque régime #####

# Amplitude entre les moyennes des régimes :

ggplot(AmplitudeMean, aes(x = year)) +
  geom_line(aes(y = leaves, color = "Leaves")) +
  geom_line(aes(y = twigs, color = "Twigs")) +
  labs(title = "Amplitude pour les feuilles et brindilles",
       x = "Date",
       y = "Amplitude de Productivité primaire (MgC_m2)",
       color = "Composante de la litière") +
  scale_color_manual(values = c("Leaves" = "green", "Twigs" = "brown"),
                     name = "Composante de la litière") +
  facet_wrap(~ fire_regime) +   theme_classic()

ggplot(AmplitudeMean, aes(x = year, y = leaves, color = fire_regime, linetype = fire_regime)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = fire_regime)) + 
  labs(title = "Droite de régression pour les feuilles",
       x = "Année",
       y = "Amplitude de Productivité primaire (MgC_m2)",
       color = "Fire Regime") +  
  scale_color_manual(values = color) +  
  scale_linetype_manual(values = linetype) +
  theme_classic()

ggplot(AmplitudeMean, aes(x = year, y = twigs,  color = fire_regime, linetype = fire_regime)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = fire_regime)) + 
  labs(title = "Droite de régression pour les brindilles",
       x = "Année",
       y = "Amplitude de Productivité primaire (MgC_m2)",
       color = "Fire Regime") +
    
  scale_color_manual(values = color) +  
  scale_linetype_manual(values = linetype) +
  theme_classic()

### Différence de jour entre les pics de chaque année #####

# Groupement par année et recherche des dates des pics de "leaves" et "twigs"
peaks <- dados %>%
  group_by(fire_regime, year = lubridate::year(date)) %>%
  summarise(date_max_leaves = date[which.max(leaves)],
            date_max_twigs = date[which.max(twigs)])

# Calculer le nombre de jours entre les dates des pics de "leaves" et "twigs"
peaks <- peaks %>% mutate(days_between_leaves = as.integer(date_max_leaves - lag(date_max_leaves)))
peaks <- peaks %>% mutate(days_between_twigs = as.integer(date_max_twigs - lag(date_max_twigs)))
peaks <- peaks[peaks$year != 2018, ]

ggplot(peaks, aes(x = year)) +
  geom_line(aes(y = days_between_leaves, color = "Leaves")) +
  geom_line(aes(y = days_between_twigs, color = "Twigs")) +
  labs(title = "Différences de jours entre les pics de productivité primaire",
       x = "Années",
       y = "Nombre de jours entre les pics",
       color = "Composante de la litière") +
  scale_color_manual(values = c("Leaves" = "green", "Twigs" = "brown"),
                     name = "Composante de la litière") +
  facet_wrap(~ fire_regime) +   theme_classic()

ggplot(peaks, aes(x = year, y = days_between_leaves,  color = fire_regime, linetype = fire_regime)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = fire_regime)) + 
  labs(title = "Droite de régression pour les feuilles",
       x = "Année",
       y = "Nombre de jours entre les pics",
       color = "Fire Regime", linetype = "Fire Regime") +
  scale_color_manual(values = color) +  
  scale_linetype_manual(values = linetype) +
  theme_classic()

ggplot(peaks, aes(x = year, y = days_between_twigs,  color = fire_regime, linetype = fire_regime)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = fire_regime)) + 
  labs(title = "Droite de régression pour les brindilles",
       x = "Année",
       y = "Nombre de jours entre les pics",
      color = "Régime de feu", linetype = "Régime de feu", linetype = "Régime de feu") +
  scale_color_manual(values = color) +  
  scale_linetype_manual(values = linetype) +
  theme_classic()
