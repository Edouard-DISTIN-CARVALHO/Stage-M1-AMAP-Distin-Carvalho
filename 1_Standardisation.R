
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#           1/ Standardisation de la productivité par l'aire basale             ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


getwd()
#setwd("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Dados") # AMAP
setwd("C:/Users/edoua/OneDrive/Documents/R/AMAP/Dados") #GalaxyBook

library(dplyr)
library(lubridate)
library(ggplot2)

dados_basal <- read.csv("ESA_basal_area.csv", header = TRUE, sep = ",", dec =".")

dados_basal$basal_area <- as.numeric(dados_basal$basal_area)
dados_basal <- mutate(dados_basal, basal_area = ifelse(is.na(basal_area), 0, basal_area))

#### Calcul des aires basales par parcelle de chaque année ####
dados_basal_tot <- dados_basal%>% group_by(plot_code, year) %>%
  summarize(somme_basal_area = sum(basal_area, na.rm = TRUE))

# Fonction pour estimer les valeurs manquantes pour chaque plot_code
missing_year <- (c("2018", "2020", "2022"))
interpolation_year<- function(df, missing_year) {
  all_year <- sort(unique(c(df$year, missing_year)))
  interpolation <- approx(df$year, df$somme_basal_area, xout = all_year)
  data.frame(year = interpolation$x, somme_basal_area = interpolation$y)
}
dados_basal_2 <- dados_basal_tot %>%
  group_by(plot_code) %>%
  do(interpolation_year(., missing_year)) %>%
  ungroup()
dados_basal_2 <- dados_basal_tot %>%
  select(plot_code) %>%
  distinct() %>%
  inner_join(dados_basal_2, by = "plot_code")

# Ajout variable fire_regime
dados_basal_2  <- dados_basal_2  %>%  mutate(fire_regime = plot_code)
names(dados_basal_2 )[names(dados_basal_2 ) == "dados_basal_2$fire_regime"] <- "fire_regime"
dados_basal_2$fire_regime <- factor(dados_basal_2$fire_regime, 
                                    levels = c("ESA-04", "ESA-05", "ESA-06", "ESA-07", "ESA-08", "ESA-09"),
                                    labels = c("control_tri", "biennial", "control_bi", "triennial", "control_an", "annual"))

color <- c("control_bi" = "green", "biennial" = "green",
           "control_tri" = "blue", "triennial" = "blue",
           "control_an" = "red", "annual" = "red")

linetype <- c("control_bi" = "solid", "biennial" = "dashed",
              "control_tri" = "solid", "triennial" = "dashed",
              "control_an" = "solid", "annual" = "dashed")

dados_basal_tot  <- dados_basal_tot  %>%  mutate(fire_regime = plot_code)
names(dados_basal_tot )[names(dados_basal_tot ) == "dados_basal_tot$fire_regime"] <- "fire_regime"
dados_basal_tot$fire_regime <- factor(dados_basal_tot$fire_regime, 
                                      levels = c("ESA-04", "ESA-05", "ESA-06", "ESA-07", "ESA-08", "ESA-09"),
                                      labels = c("control_tri", "biennial", "control_bi", "triennial", "control_an", "annual"))


ggplot(dados_basal_tot, aes(x = year, y = somme_basal_area, color = fire_regime, 
                          linetype = fire_regime)) +  geom_point() +  geom_line() +   
  scale_color_manual(values = color) + scale_linetype_manual(values = linetype) +
  scale_x_continuous(breaks = seq(2016, 2023, by = 1)) +
  labs(title = "Evolution de l'aire basale avec prédiction au cours du temps", 
       x = "Années", y = "Aire Basale (m²/y)", color = "Type de parcelle", 
       linetype = "Type de parcelle") +  
  theme_classic()

ggplot(dados_basal_2, aes(x = year, y = somme_basal_area, 
                          color = fire_regime, 
                          linetype = fire_regime)) + 
  geom_line(aes(group = interaction(fire_regime, plot_code))) +
  geom_point(aes(group = interaction(fire_regime, plot_code))) +
  scale_color_manual(values = color) + 
  scale_linetype_manual(values = linetype) +
  labs(title = "Evolution de l'aire basale avec prédiction au cours du temps", 
       x = "Années", y = "Aire Basale (m²/y)", color = "Type de parcelle", 
       linetype = "Type de parcelle") +  
  theme_classic()

#### Application au donnée de productivité primaire ####
dados_brutos <- read.csv("ESA_litterfall_NPP.csv", header = TRUE, sep = ",", dec =".") 

# suppression erreur donnée
dados <- dados_brutos[-5840, ]

# Suppresion des NA et donnés 2024
dados <- na.omit(dados)
dados <- dados[dados$year != 2024, ]

# Création d'une nouvelle colonne basé sur le régime de feu
dados$date <- as.Date(dados$date, format = "%d/%m/%Y")
dados <- dados %>%  mutate(fire_regime = plot_code)
names(dados)[names(dados) == "dados$fire_regime"] <- "fire_regime"
dados$fire_regime <- factor(dados$fire_regime,
                            levels = c("ESA-04", "ESA-05", "ESA-06", "ESA-07", "ESA-08", "ESA-09"),
                            labels = c("control_tri", "biennial", "control_bi", "triennial", "control_an", "annual"))

# Boucle de Normalisation : NPP/Aire Basale pour chaque parcelle de chaque année
dados_norm <- dados

for (year in unique(dados_basal_2$year)) {
  if (year %in% dados_norm$year && year %in% dados_basal_2$year) {
    for (plot_code in unique(dados_basal_2$plot_code)) {
      if (plot_code %in% dados_norm$plot_code && plot_code %in% dados_basal_2$plot_code) {
        # Sélectionner la somme_basal_area correspondante
        somme_basal_area <- dados_basal_2 %>%
          filter(year == !!year, plot_code == !!plot_code) %>%
          pull(somme_basal_area)
        
        if (length(somme_basal_area) == 1) {
          # Normaliser les colonnes 8 à 14
          dados_norm[dados_norm$year == year & dados_norm$plot_code == plot_code, 8:14] <-
            dados_norm[dados_norm$year == year & dados_norm$plot_code == plot_code, 8:14] / somme_basal_area
        }
      }
    }
  }
}


# Donnée normalisée
dados_norm$date <- as.Date(dados_norm$date, format = "%d/%m/%Y")
ggplot(dados_norm, aes(x = date, y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_smooth(method = "loess", span = 0.2)  + 
  labs(title = "Evolution de la productivité primaire totale normalisée",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_m2)", 
       color = "Regime de feu") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + theme_classic() 

ggplot(dados_norm, aes(x = date, y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_point()  + 
  labs(title = "Evolution de la productivité primaire totale normalisée",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_m2)", 
       color = "Regime de feu") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + theme_classic() 

# Données brute 
ggplot(dados, aes(x = date, y = total_litterfall_MgC_ha_year, color = fire_regime)) +
  geom_smooth(method = "loess", span = 0.2)  + 
  labs(title = "Evolution de la productivité primaire totale brute",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_ha_year)",
       color ="Regime de feu") + scale_x_date(date_breaks = "1 year", 
                                               date_labels = "%Y") + theme_classic()

ggplot(dados, aes(x = date, y = total_litterfall_MgC_ha_year, color = fire_regime)) +
  geom_point()  + 
  labs(title = "Evolution de la productivité primaire totale brute",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_ha_year)",
       color ="Regime de feu") + scale_x_date(date_breaks = "1 year", 
                                              date_labels = "%Y") + theme_classic()

