#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#                          2/ Composition des litières                            ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyr)
library(ggplot2)

source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/1_Normalisation.R")

names(dados_norm)[8:14] <- c("leaves", "twigs", "flower", 
                             "fruits", "seeds", "outros", "total_MgC_m2")
dados_norm$fire_regime <- factor(dados_norm$fire_regime, levels = c("annual", "biennial", "triennial", 
  "control_an", "control_bi", "control_tri"))


##### Pourcentage au fil du temps et des régimes de feu #### 

MeanFire100 <- dados_norm %>%
  group_by(year = lubridate::year(date), 
           month = lubridate::month(date),
           fire_regime) %>%
  summarise(leaves = mean(leaves, na.rm = TRUE),
            twigs = mean(twigs, na.rm = TRUE),
            flower = mean(flower, na.rm = TRUE),
            fruits = mean(fruits, na.rm = TRUE),
            seeds = mean(seeds, na.rm = TRUE),
            outros = mean(outros, na.rm = TRUE),
            total_MgC_m2 = mean(total_MgC_m2, na.rm = TRUE)) %>%
  ungroup()

dados100_fire <- mutate(MeanFire100, 
                        leaves = leaves / total_MgC_m2 * 100,
                        twigs = twigs / total_MgC_m2 * 100,
                        flower = flower / total_MgC_m2 * 100,
                        fruits = fruits / total_MgC_m2 * 100,
                        seeds = seeds / total_MgC_m2 * 100,
                        outros = outros / total_MgC_m2 * 100)
dados100_fire$date <- as.Date(paste(dados100_fire$year, dados100_fire$month, "01", sep = "-"))

ggplot(dados100_fire, aes(x = date)) +
  geom_bar(aes(y = leaves, fill = "Leaves"), stat = "identity", position = "stack") +
  geom_bar(aes(y = twigs, fill = "Twigs"), stat = "identity", position = "stack") +
  geom_bar(aes(y = flower, fill = "Flower"), stat = "identity", position = "stack") +
  geom_bar(aes(y = fruits, fill = "Fruits"), stat = "identity", position = "stack") +
  geom_bar(aes(y = seeds, fill = "Seeds"), stat = "identity", position = "stack") +
  geom_bar(aes(y = outros, fill = "Others"), stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Leaves" = "green", "Twigs" = "brown", "Flower" = "pink", 
                               "Fruits" = "red", "Seeds" = "black", "Others" = "gray"),
                    name = "Composante de la litière") + 
  labs(title = "Pourcentage des composants de la litière en fonction des années",
       x = "Année",
       y = "Pourcentage",
       fill = "Composante de la litière") +  facet_wrap(~ fire_regime) +
  theme_minimal()

##### Moyenne au fil du temps #####

# Extraire les moyennes pour chaque mois de chaque année pour chaque variable
Mean <- dados_norm %>%
  group_by(year = lubridate::year(date), month = lubridate::month(date)) %>%
  summarise(leaves_mean = mean(leaves, na.rm = TRUE),
            twigs_mean = mean(twigs, na.rm = TRUE),
            flower_mean = mean(flower, na.rm = TRUE),
            fruits_mean = mean(fruits, na.rm = TRUE),
            seeds_mean = mean(seeds, na.rm = TRUE),
            outros_mean = mean(outros, na.rm = TRUE)) %>%
  ungroup()

# Filtrer les données pour les mois de juin à octobre et de décembre à mars
dados_mean <- subset(Mean, month %in% 6:10 | month %in% c(12, 1, 2, 3))

# Calculer la différence entre les valeurs des mois de juin à octobre et de décembre à mars pour chaque année et chaque variable
AmplitudeMean <- dados_mean %>%
  group_by(year) %>%
  summarise(AmplitudeMean_leaves = max(leaves_mean) - min(leaves_mean),
            AmplitudeMean_twigs = max(twigs_mean) - min(twigs_mean),
            AmplitudeMean_flower = max(flower_mean) - min(flower_mean),
            AmplitudeMean_fruits = max(fruits_mean) - min(fruits_mean),
            AmplitudeMean_seeds = max(seeds_mean) - min(seeds_mean),
            AmplitudeMean_outros = max(outros_mean) - min(outros_mean))

# Graphique pour les valeurs moyenne
Mean$date <- as.Date(paste(Mean$year, Mean$month, "01", sep = "-"))
CompoMean <- pivot_longer(Mean, cols = 3:8, names_to = "Composant", values_to = "Productivite")

ggplot(CompoMean, aes(x = date, y = Productivite, color = Composant)) +
  geom_line() +
  labs(title = "Moyenne de Productivité primaire total des composantes des litières",
       x = "Date de collecte",
       y = "Moyenne de Productivité primaire (MgC_m2)",
       color = "Composant de la litière") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic()

# Graphique pour les AmplitudeMeans
CompoAmpMean <- pivot_longer(AmplitudeMean, cols = 2:7, names_to = "Composant",
                             values_to = "AmplitudeMean")
ggplot(CompoAmpMean, aes(x = year, y = AmplitudeMean, color = Composant)) +
  geom_line() +
  labs(title = "Amplitude Moyenn de Productivité primaire des composantes des litières",
       x = "Date de collecte",
       y = "Amplitude Moyen de Productivité primaire (MgC_m2)",
       color = "Composant de la litière") +
  theme_classic()

#### Effet des régimes de feu ####

# Extraire les moyennes de chaque régime pour chaque mois de chaque année pour chaque parcelle
MeanFire <- dados_norm %>%
  group_by(year = lubridate::year(date), 
           month = lubridate::month(date),
           fire_regime) %>%
  summarise(leaves_meanfire =  mean(leaves, na.rm = TRUE),
            twigs_meanfire =  mean(twigs, na.rm = TRUE),
            flower_meanfire =  mean(flower, na.rm = TRUE),
            fruits_meanfire =  mean(fruits, na.rm = TRUE),
            seeds_meanfire =  mean(seeds, na.rm = TRUE),
            outros_meanfire =  mean(outros, na.rm = TRUE)) %>%
  ungroup()

MeanFire$date <- as.Date(paste(MeanFire$year, MeanFire$month, "01", sep = "-"))
ggplot(MeanFire, aes(x = date)) +
  geom_line(aes(y = leaves_meanfire, color = "Leaves")) +
  geom_line(aes(y = twigs_meanfire, color = "Twigs")) +
  geom_line(aes(y = flower_meanfire, color = "Flower")) +
  geom_line(aes(y = fruits_meanfire, color = "Fruits")) +
  geom_line(aes(y = seeds_meanfire, color = "Seeds")) +
  geom_line(aes(y = outros_meanfire, color = "Others")) +
  labs(title = "Moyenne de Productivité primaire des composantes de la litière selon les régimes de feu",
       x = "Date",
       y = "Moyenne de Productivité primaire (MgC_m2)",
       color = "Composante de la litière") +
  scale_color_manual(values = c("Leaves" = "green", "Twigs" = "brown", "Flower" = "pink", 
                                "Fruits" = "red", "Seeds" = "black", "Others" = "gray"),
                     name = "Composante de la litière") +
  facet_wrap(~ fire_regime) +
  theme_minimal()




