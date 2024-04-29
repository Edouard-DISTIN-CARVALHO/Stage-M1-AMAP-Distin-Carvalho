#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#       3/ Etude de la saisonalité de la productivité primaire                  ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/1_Normalisation.R") 
source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/2_Composition.R") 

library(lubridate)
library(ade4)
library(lme4)
library(ggplot2)

dados_fire <- dados_norm %>%  filter(plot_code %in% c("ESA-05", "ESA-07", "ESA-09"))
dados_fire$fire_regime <- as.factor(dados_fire$fire_regime)
dados_fire$year <- as.factor(dados_fire$year)

#### Modèle sur l'effet du feu sur les composantes ####

##### Total #####
mod_total <- lm(log10(total_MgC_m2+0.1) ~ fire_regime*year+(1|sub_plot),
                data=dados_fire)
hsd_total <- lsmeans(mod_total, pairwise~fire_regime*year)
plot(hsd_total)+labs(title = "Résultat Test de Tukey HSD totale")
rs_total <- subset(summary(hsd_total)$contrasts, p.value < 0.05)
rns_total <- subset(summary(hsd_total)$contrasts, p.value > 0.05)



