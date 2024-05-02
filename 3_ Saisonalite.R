#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#       3/ Etude de la saisonalité de la productivité primaire                  ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/1_Standardisation.R") 
source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/2_Composition.R") 

library(lubridate)
library(dplyr)
library(ade4)
library(lme4)
library(ggplot2)

##### Différence d'amplitude moyenne entre les pics de chaque année pour chaque régime #####

# Amplitude entre les moyennes des régimes :
AmplitudeFire <- MeanFire %>%
  mutate(month = lubridate::month(date)) %>%
  filter((month >= 6 & month <= 10) | (month >= 12 | month <= 3)) %>%
  group_by(year = lubridate::year(date), fire_regime) %>%
  summarise(Amplitude_leaves = max(leaves_meanfire) - min(leaves_meanfire),
            Amplitude_twigs = max(twigs_meanfire) - min(twigs_meanfire)) %>%
  ungroup()

ggplot(AmplitudeFire, aes(x = year)) +
  geom_line(aes(y = Amplitude_leaves, color = "Leaves")) +
  geom_line(aes(y = Amplitude_twigs, color = "Twigs")) +
  labs(title = "Amplitude pour les feuilles et brindilles",
       x = "Date",
       y = "Amplitude de Productivité primaire (MgC_m2)",
       color = "Composante de la litière") +
  scale_color_manual(values = c("Leaves" = "green", "Twigs" = "brown"),
                     name = "Composante de la litière") +
  facet_wrap(~ fire_regime) +   theme_classic()

ggplot(AmplitudeFire, aes(x = year, y = Amplitude_leaves, shape = fire_regime, color = fire_regime, linetype = fire_regime)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = fire_regime)) + 
  labs(title = "Droite de régression pour les feuilles",
       x = "Année",
       y = "Amplitude de Productivité primaire (MgC_m2)",
       color = "Fire Regime") +
  scale_shape_manual(values = c(19,15,17,1,22,24)) +  
  scale_color_manual(values = color) +  
  scale_linetype_manual(values = linetype) +
  theme_classic()

ggplot(AmplitudeFire, aes(x = year, y = Amplitude_twigs, shape = fire_regime, color = fire_regime, linetype = fire_regime)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = fire_regime)) + 
  labs(title = "Droite de régression pour les brindilles",
       x = "Année",
       y = "Amplitude de Productivité primaire (MgC_m2)",
       color = "Fire Regime") +
  scale_shape_manual(values = c(19,15,17,1,22,24)) +  
  scale_color_manual(values = color) +  
  scale_linetype_manual(values = linetype) +
  theme_classic()


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
peaks <- peaks[peaks$year != 2018, ]

#peaks <- mutate(peaks, days_between_leaves = ifelse(is.na(days_between_leaves), 0, days_between_leaves))
#peaks <- peaks %>% mutate(days_between_twigs = as.integer(date_max_twigs - lag(date_max_twigs)))
#peaks <- mutate(peaks, days_between_twigs = ifelse(is.na(days_between_twigs), 0, days_between_twigs))

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

ggplot(peaks, aes(x = year, y = days_between_leaves, shape = fire_regime, color = fire_regime, linetype = fire_regime)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, aes(group = fire_regime)) + 
  labs(title = "Droite de régression pour les feuilles",
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
  labs(title = "Droite de régression pour les brindilles",
       x = "Année",
       y = "Nombre de jours entre les pics",
       color = "Fire Regime") +
  scale_shape_manual(values = c(19,15,17,1,22,24)) +  
  scale_color_manual(values = color) +  
  scale_linetype_manual(values = linetype) +
  theme_classic()
