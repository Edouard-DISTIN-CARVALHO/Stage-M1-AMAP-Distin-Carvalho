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


#### Composition au fil du temps #### 

# Mettre en forme les données dans un format long
dados_comp <- pivot_longer(dados_norm, cols = 8:13, names_to = "Composant", values_to = "Productivite")

# Convertir la colonne "date" en format de date
dados_comp$date <- as.Date(dados_comp$date, format = "%d/%m/%Y")

ggplot(dados_comp, aes(x = date, y = Productivite, color = Composant,shape = Composant)) +
  geom_point(size=3) +
  labs(title = "Productivité primaire des composantes de la litières au cours du temps",
       x = "Date de collecte",
       y = "Productivité primaire (MgC_m2)",
       color = "Composant de la litière") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic()

##### Moyenne #####

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
ggplot(CompoMean, aes(x = date, y = Productivite, color = Composant,shape = Composant)) +
  geom_point(size=3) +
  labs(title = "Moyenne de Productivité primaire total des composantes des litières",
       x = "Date de collecte",
       y = "Moyenne de Productivité primaire (MgC_m2)",
       color = "Composant de la litière") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic()

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

##### Maximale #####

# Extraire les valeurs maximales pour chaque mois de chaque année
Max <- dados_norm %>%
  group_by(year = lubridate::year(date), month = lubridate::month(date)) %>%
  summarise(leaves_max = max(leaves, na.rm = TRUE),
            twigs_max = max(twigs, na.rm = TRUE),
            flower_max = max(flower, na.rm = TRUE),
            fruits_max = max(fruits, na.rm = TRUE),
            seeds_max = max(seeds, na.rm = TRUE),
            outros_max = max(outros, na.rm = TRUE)) %>%
  ungroup()

# Filtrer les données pour les mois de juin à octobre et de décembre à mars
dados_max <- subset(Max, month %in% 6:10 | month %in% c(12, 1, 2, 3))

# Calculer la différence entre les valeurs des mois de juin à octobre et de décembre à mars pour chaque année et chaque variable
AmplitudeMax <- dados_max %>%
  group_by(year) %>%
  summarise(amplitude_leaves = max(leaves_max) - min(leaves_max),
            amplitude_twigs = max(twigs_max) - min(twigs_max),
            amplitude_flower = max(flower_max) - min(flower_max),
            amplitude_fruits = max(fruits_max) - min(fruits_max),
            amplitude_seeds = max(seeds_max) - min(seeds_max),
            amplitude_outros = max(outros_max) - min(outros_max))

# Graphique pour les valeurs maximales
Max$date <- as.Date(paste(Max$year, Max$month, "01", sep = "-"))
CompoMax <- pivot_longer(Max, cols = 3:8, names_to = "Composant", values_to = "Productivite")
ggplot(CompoMax, aes(x = date, y = Productivite, color = Composant,shape = Composant)) +
  geom_point(size=3) +
  labs(title = "Productivité primaire total maximale",
       x = "Date de collecte",
       y = "Productivité primaire (MgC_m2)",
       color = "Composant de la litière") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic()

ggplot(CompoMax, aes(x = date, y = Productivite, color = Composant)) +
  geom_line() +
  labs(title = "Productivité primaire maximale",
       x = "Date de collecte",
       y = "Productivité primaire (MgC_m2)",
       color = "Composant de la litière") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_classic()

# Graphique pour les amplitudes
CompoAmpMax <- pivot_longer(AmplitudeMax, cols = 2:7, names_to = "Composant", values_to = "Amplitude")
ggplot(CompoAmpMax, aes(x = year, y = Amplitude, color = Composant, shape = Composant)) +
  geom_point(size=3) +
  labs(title = "Amplitude de Productivité primaire Maximale",
       x = "Date de collecte",
       y = "Amplitude de Productivité primaire Maximale (MgC_m2)",
       color = "Composant de la litière") +
  theme_classic()

ggplot(CompoAmpMax, aes(x = year, y = Amplitude, color = Composant)) +
  geom_line() +
  labs(title = "Amplitude de Productivité primaire totale",
       x = "Date de collecte",
       y = "Amplitude de Productivité primaire (MgC_m2)",
       color = "Composant de la litière") +
  theme_classic()

ggplot(CompoAmpMax, aes(x = year, y = Amplitude, color = Composant)) +
  geom_smooth(method=lm) +
  labs(title = "Amplitude de Productivité primaire Maximale",
       x = "Date de collecte",
       y = "Amplitude de Productivité primaire Maximale (MgC_m2)",
       color = "Composant de la litière") +
  theme_classic()

##### Pourcentage #####

Mean100 <- dados_norm %>%
  group_by(year = lubridate::year(date), month = lubridate::month(date)) %>%
  summarise(leaves = mean(leaves, na.rm = TRUE),
            twigs = mean(twigs, na.rm = TRUE),
            flower = mean(flower, na.rm = TRUE),
            fruits = mean(fruits, na.rm = TRUE),
            seeds = mean(seeds, na.rm = TRUE),
            outros = mean(outros, na.rm = TRUE),
            total_MgC_m2 = mean(total_MgC_m2, na.rm = TRUE)) %>%
  ungroup()

dados100 <- mutate(Mean100, 
                   leaves = leaves / total_MgC_m2 * 100,
                   twigs = twigs / total_MgC_m2 * 100,
                   flower = flower / total_MgC_m2 * 100,
                   fruits = fruits / total_MgC_m2 * 100,
                   seeds = seeds / total_MgC_m2 * 100,
                   outros = outros / total_MgC_m2 * 100)
dados100$date <- as.Date(paste(dados100$year, dados100$month, "01", sep = "-"))
ggplot(dados100, aes(x = date)) +
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
       fill = "Composante de la litière") + 
  theme_minimal()

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

#### Effet des régimes de feu ####

ggplot(dados_norm, aes(x = date)) +
  geom_point(aes(y = leaves, shape = "Leaves"), color = "green") +
  geom_point(aes(y = twigs, shape = "Twigs"), color = "brown") +
  geom_point(aes(y = flower, shape = "Flower"), color = "pink") +
  geom_point(aes(y = fruits, shape = "Fruits"), color = "red") +
  geom_point(aes(y = seeds, shape = "Seeds"), color = "black") +
  geom_point(aes(y = outros, shape = "Others"), color = "gray") +
  labs(title = "Productivité primaire des composante de la litière selon les régimes de feu",
       x = "Date",
       y = "Productivité primaire (Mcg_m2)",
       shape = "Composante de la litière") +
  scale_shape_manual(values = c("Leaves" = 1, "Twigs" = 2, "Flower" = 3, 
                                "Fruits" = 4, "Seeds" = 5, "Others" = 6),
                     name = "Composante de la litière") +
  facet_wrap(~ fire_regime) +  theme_minimal()

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

# Amplitude entre les moyennes des régimes :
AmplitudeFire <- MeanFire %>%
  mutate(month = lubridate::month(date)) %>%
  filter((month >= 6 & month <= 10) | (month >= 12 | month <= 3)) %>%
  group_by(year = lubridate::year(date), fire_regime) %>%
  summarise(Amplitude_leaves = max(leaves_meanfire) - min(leaves_meanfire),
            Amplitude_twigs = max(twigs_meanfire) - min(twigs_meanfire),
            Amplitude_flower = max(flower_meanfire) - min(flower_meanfire),
            Amplitude_fruits = max(fruits_meanfire) - min(fruits_meanfire),
            Amplitude_seeds = max(seeds_meanfire) - min(seeds_meanfire),
            Amplitude_outros = max(outros_meanfire) - min(outros_meanfire)) %>%
  ungroup()

ggplot(AmplitudeFire, aes(x = year)) +
  geom_line(aes(y = Amplitude_leaves, color = "Leaves")) +
  geom_line(aes(y = Amplitude_twigs, color = "Twigs")) +
  geom_line(aes(y = Amplitude_flower, color = "Flower")) +
  geom_line(aes(y = Amplitude_fruits, color = "Fruits")) +
  geom_line(aes(y = Amplitude_seeds, color = "Seeds")) +
  geom_line(aes(y = Amplitude_outros, color = "Others")) +
  labs(title = "Amplitude entre les Productivité primaire des composante de la litière selon les régimes de feu par an",
       x = "Date",
       y = "Amplitude de Productivité primaire (MgC_m2)",
       color = "Composante de la litière") +
  scale_color_manual(values = c("Leaves" = "green", "Twigs" = "brown", "Flower" = "pink", 
                                "Fruits" = "red", "Seeds" = "black", "Others" = "gray"),
                     name = "Composante de la litière") +
  facet_wrap(~ fire_regime) +   theme_minimal()

# Extraire le valeurs maximums pour chaque mois de chaque année pour chaque parcelle
maxFire <- dados_norm %>%
  group_by(year = lubridate::year(date), 
           month = lubridate::month(date),
           fire_regime) %>%
  summarise(leaves_maxfire =  max(leaves, na.rm = TRUE),
            twigs_maxfire =  max(twigs, na.rm = TRUE),
            flower_maxfire =  max(flower, na.rm = TRUE),
            fruits_maxfire =  max(fruits, na.rm = TRUE),
            seeds_maxfire =  max(seeds, na.rm = TRUE),
            outros_maxfire =  max(outros, na.rm = TRUE)) %>%
  ungroup()

maxFire$date <- as.Date(paste(maxFire$year, maxFire$month, "01", sep = "-"))
ggplot(maxFire, aes(x = date)) +
  geom_line(aes(y = leaves_maxfire, color = "Leaves")) +
  geom_line(aes(y = twigs_maxfire, color = "Twigs")) +
  geom_line(aes(y = flower_maxfire, color = "Flower")) +
  geom_line(aes(y = fruits_maxfire, color = "Fruits")) +
  geom_line(aes(y = seeds_maxfire, color = "Seeds")) +
  geom_line(aes(y = outros_maxfire, color = "Others")) +
  labs(title = "Productivité primaire Maximale des composantes de la litière selon les régimes de feu",
       x = "Date",
       y = "Productivité primaire Maximale (MgC_m2)",
       color = "Composante de la litière") +
  scale_color_manual(values = c("Leaves" = "green", "Twigs" = "brown", "Flower" = "pink", 
                                "Fruits" = "red", "Seeds" = "black", "Others" = "gray"),
                     name = "Composante de la litière") +
  facet_wrap(~ fire_regime) +
  theme_minimal()

# Amplitude entre valeurs max des régimes de feu:

AmplitudeMaxFire <- maxFire %>%
  mutate(month = lubridate::month(date)) %>%
  filter((month >= 6 & month <= 10) | (month >= 12 | month <= 3)) %>%
  group_by(year = lubridate::year(date), fire_regime) %>%
  summarise(Amplitude_leaves = max(leaves_maxfire) - min(leaves_maxfire),
            Amplitude_twigs = max(twigs_maxfire) - min(twigs_maxfire),
            Amplitude_flower = max(flower_maxfire) - min(flower_maxfire),
            Amplitude_fruits = max(fruits_maxfire) - min(fruits_maxfire),
            Amplitude_seeds = max(seeds_maxfire) - min(seeds_maxfire),
            Amplitude_outros = max(outros_maxfire) - min(outros_maxfire)) %>%
  ungroup()

ggplot(AmplitudeMaxFire, aes(x = year)) +
  geom_line(aes(y = Amplitude_leaves, color = "Leaves")) +
  geom_line(aes(y = Amplitude_twigs, color = "Twigs")) +
  geom_line(aes(y = Amplitude_flower, color = "Flower")) +
  geom_line(aes(y = Amplitude_fruits, color = "Fruits")) +
  geom_line(aes(y = Amplitude_seeds, color = "Seeds")) +
  geom_line(aes(y = Amplitude_outros, color = "Others")) +
  labs(title = "Amplitude de Productivité primaire Maximale des composante de la litière selon les régimes de feu",
       x = "Date",
       y = "Amplitude Productivité primaire (MgC_m2)",
       color = "Composante de la litière") +
  scale_color_manual(values = c("Leaves" = "green", "Twigs" = "brown", "Flower" = "pink", 
                                "Fruits" = "red", "Seeds" = "black", "Others" = "gray"),
                     name = "Composante de la litière") +
  facet_wrap(~ fire_regime) +   theme_minimal() 


