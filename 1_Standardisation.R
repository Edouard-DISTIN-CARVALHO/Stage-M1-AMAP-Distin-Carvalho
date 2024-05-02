
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#           1/ Standardisation de la productivité par l'aire basale             ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


getwd()
setwd("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Dados") # AMAP
#setwd("C:/Users/edoua/OneDrive/Documents/R/AMAP") #GalaxyBook


dados_basal <- read.csv("ESA_basal_area.csv", header = TRUE, sep = ",", dec =".")

library(dplyr)
dados_basal$basal_area <- as.numeric(dados_basal$basal_area)
dados_basal <- mutate(dados_basal, basal_area = ifelse(is.na(basal_area), 0, basal_area))

#### Calcul des aires basales par parcelle de chaque année ####
dados_basal_tot <- dados_basal%>% group_by(plot_code, year) %>%
  summarize(somme_basal_area = sum(basal_area, na.rm = TRUE))

# Fonction pour estimer les valeurs manquantes pour chaque plot_code
impute_missing <- function(data) {
  # Modèle de régression linéaire pour estimer les valeurs manquantes
  lm_model <- lm(somme_basal_area ~ year, data = data)
  
  # Ensemble de données avec les années manquantes
  missing_years <- data.frame(year = c(2018, 2020, 2022))
  
  # Prédire les valeurs manquantes de basal_area
  predictions <- predict(lm_model, newdata = missing_years)
  missing_years$somme_basal_area <- predictions
  
  return(missing_years)}

# Appliquer la fonction pour chaque ensemble de données plot_code
dados_basal_est <- dados_basal_tot %>%  do(impute_missing(.))

# Fusionner les jeux de données
dados_basal_2 <- merge(dados_basal_est, dados_basal_tot, by = c("plot_code", "year","somme_basal_area"), all = TRUE)

dados_basal_2  <- dados_basal_2  %>%  mutate(fire_regime = plot_code)
names(dados_basal_2 )[names(dados_basal_2 ) == "dados_basal_2$fire_regime"] <- "fire_regime"
dados_basal_2$fire_regime <- factor(dados_basal_2$fire_regime, 
                                    levels = c("ESA-04", "ESA-05", "ESA-06", "ESA-07", "ESA-08", "ESA-09"),
                                    labels = c("control_bi", "biennial", "control_tri", "triennial", "control_an", "annual"))

color <- c("control_bi" = "green", "biennial" = "green",
           "control_tri" = "blue", "triennial" = "blue",
           "control_an" = "red", "annual" = "red")

linetype <- c("control_bi" = "solid", "biennial" = "dashed",
              "control_tri" = "solid", "triennial" = "dashed",
              "control_an" = "solid", "annual" = "dashed")

library(ggplot2)
dados_basal_tot  <- dados_basal_tot  %>%  mutate(fire_regime = plot_code)
names(dados_basal_tot )[names(dados_basal_tot ) == "dados_basal_tot$fire_regime"] <- "fire_regime"
dados_basal_tot$fire_regime <- factor(dados_basal_tot$fire_regime, 
                                      levels = c("ESA-04", "ESA-05", "ESA-06", "ESA-07", "ESA-08", "ESA-09"),
                                      labels = c("control_bi", "biennial", "control_tri", "triennial", "control_an", "annual"))
ggplot(dados_basal_tot, aes(x = year, y = somme_basal_area, color = fire_regime, 
                          linetype = fire_regime)) +  geom_point() +  geom_line() +   
  scale_color_manual(values = color) + scale_linetype_manual(values = linetype) +
  scale_x_continuous(breaks = seq(2016, 2023, by = 1)) +
  labs(title = "Evolution de l'aire basale avec prédiction au cours du temps", 
       x = "Années", y = "Aire Basale (m²/y)", color = "Type de parcelle") +  
  theme_classic()

ggplot(dados_basal_2, aes(x = year, y = somme_basal_area, color = fire_regime, 
                          linetype = fire_regime)) +  geom_point() +  geom_line() +   
  scale_color_manual(values = color) + scale_linetype_manual(values = linetype) +
  scale_x_continuous(breaks = seq(2016, 2023, by = 1)) +
  labs(title = "Evolution de l'aire basale avec prédiction au cours du temps", 
       x = "Années", y = "Aire Basale (m²/y)", color = "Type de parcelle") +  
  theme_classic()

#### Application au donnée de productivité primaire ####
dados <- read.csv("ESA_litterfall_NPP.csv", header = TRUE, sep = ",", dec =".") 

# Suppresion des NA et donnés 2024
dados <- na.omit(dados)
dados <- dados[dados$year != 2024, ]

# Création d'une nouvelle colonne basé sur le régime de feu
dados$date <- as.Date(dados$date, format = "%d/%m/%Y")
dados <- dados %>%  mutate(fire_regime = plot_code)
names(dados)[names(dados) == "dados$fire_regime"] <- "fire_regime"
dados$fire_regime <- factor(dados$fire_regime,
                            levels = c("ESA-04", "ESA-05", "ESA-06", "ESA-07", "ESA-08", "ESA-09"),
                            labels = c("control_bi", "biennial", "control_tri", "triennial", "control_an", "annual"))

# Boucle de Normalisation : NPP/Aire Basale pour chaque parcelle de chaque année 
dados_norm <- mutate(dados)

for (year in unique(dados_basal_2$year)) {
  if (year %in% dados_norm$year && year %in% dados_basal_2$year) {
    for (plot_code in unique(dados_basal_2$plot_code)) {
      if (plot_code %in% dados_norm$plot_code && plot_code %in% dados_basal_2$plot_code) {
        dados_norm[dados_norm$year == year & dados_norm$plot_code == plot_code, 8:14] <-
          dados_norm[dados_norm$year == year & dados_norm$plot_code == plot_code, 8:14] /
          dados_basal_2[dados_basal_2$year == year & dados_basal_2$plot_code == plot_code, "somme_basal_area"]
      }
    }
  }
}

# Donnée normalisée
dados_norm$date <- as.Date(dados_norm$date, format = "%d/%m/%Y")
ggplot(dados_norm, aes(x = date, y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_smooth(method = "gam")  + 
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
  geom_smooth(method = "gam")  + 
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
