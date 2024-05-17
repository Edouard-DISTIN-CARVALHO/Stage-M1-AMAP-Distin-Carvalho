#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#                          2/ Etude de la composition des litières                            ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(lubridate)
library(tidyr)
library(ggplot2)

getwd()
#setwd("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git") # AMAP
setwd("C:/Users/edoua/OneDrive/Documents/R/AMAP/Git") # Galaxybook
source("1_Standardisation.R")

names(dados)[8:14] <- c("leaves", "twigs", "flower", 
                             "fruits", "seeds", "outros", "total")
names(dados_norm)[8:14] <- c("leaves", "twigs", "flower", 
                        "fruits", "seeds", "outros", "total")
dados$fire_regime <- factor(dados$fire_regime, levels = c("annual", "biennial", "triennial", 
  "control_an", "control_bi", "control_tri"))

dados_norm$fire_regime <- factor(dados_norm$fire_regime, levels = c("annual", "biennial", "triennial", 
                                                          "control_an", "control_bi", "control_tri"))


##### Pourcentage au fil du temps et des régimes de feu #### 

MeanFire100 <- dados %>%
  group_by(year = lubridate::year(date), 
           month = lubridate::month(date),
           fire_regime) %>%
  summarise(leaves = mean(leaves, na.rm = TRUE),
            twigs = mean(twigs, na.rm = TRUE),
            flower = mean(flower, na.rm = TRUE),
            fruits = mean(fruits, na.rm = TRUE),
            seeds = mean(seeds, na.rm = TRUE),
            outros = mean(outros, na.rm = TRUE),
            total = mean(total, na.rm = TRUE)) %>%
  ungroup()

dados100_fire <- mutate(MeanFire100, 
                        leaves = leaves / total * 100,
                        twigs = twigs / total * 100,
                        flower = flower / total * 100,
                        fruits = fruits / total * 100,
                        seeds = seeds / total * 100,
                        outros = outros / total * 100)
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

# Extraire les moyennes pour chaque date de collecte et chaque régime de feu
Mean <- dados_norm%>%
  group_by(date = as.Date(date), fire_regime) %>%
  summarise(leaves = mean(leaves, na.rm = TRUE),
            twigs = mean(twigs, na.rm = TRUE),
            flower = mean(flower, na.rm = TRUE),
            fruits = mean(fruits, na.rm = TRUE),
            seeds = mean(seeds, na.rm = TRUE),
            outros = mean(outros, na.rm = TRUE),
            total = mean(total, na.rm = TRUE)) %>%
  ungroup()

# Graphique pour les valeurs moyenne
Mean$date <- as.Date(Mean$date,format = "%Y-%m-%d")
CompoMean <- pivot_longer(Mean, cols = 3:8, names_to = "Composant", values_to = "Productivite")
ggplot(CompoMean, aes(x = date, y = Productivite, color = Composant)) +
  geom_line() +
  labs(title = "Moyenne de Productivité primaire total des composantes des litières",
       x = "Date de collecte",
       y = "Moyenne de Productivité primaire (MgC_m2)",
       color = "Composant de la litière") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  facet_wrap(~ fire_regime) +
  theme_classic()

# Calculer la différence entre les valeurs extrèmes pour chaque regime de feu
Mean <- Mean %>%  mutate(year = year(date))
AmplitudeMean <- Mean %>%
  group_by(year, fire_regime) %>%
  summarise(leaves = max(leaves) - min(leaves),
            twigs = max(twigs) - min(twigs),
            flower = max(flower) - min(flower),
            fruits = max(fruits) - min(fruits),
            seeds = max(seeds) - min(seeds),
            outros = max(outros) - min(outros),
            total = max(total) - min(total))

CompoAmpMean <- pivot_longer(AmplitudeMean, cols = 3:8, names_to = "Composant",
                             values_to = "AmplitudeMean")
ggplot(CompoAmpMean, aes(x = year, y = AmplitudeMean, color = Composant)) +
  geom_line() +
  labs(title = "Amplitude Moyenne de Productivité primaire des composantes des litières",
       x = "Date de collecte",
       y = "Amplitude Moyen de Productivité primaire (MgC_m2)",
       color = "Composant de la litière") + 
  scale_color_manual(values = c("leaves" = "green", "twigs" = "brown", "flower" = "pink", 
                                "fruits" = "red", "seeds" = "black", "others" = "gray"),
                     name = "Composante de la litière") +
  facet_wrap(~ fire_regime) +
  theme_classic()


