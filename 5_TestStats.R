#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#                          5/ Test Statistiques                                 ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getwd()
#setwd("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git") # AMAP
setwd("C:/Users/edoua/OneDrive/Documents/R/AMAP/Git") # Galaxybook
source("4_Modelisation.R")

##### Paramètre des modèles #### 

# Regime de feu : 
dados_comp <- Mean %>%
  filter(fire_regime %in% c("annual","biennial","triennial"))
with(data=dados_comp, plot(x=date, y = leaves, main = "Fire regime Total Litterfall", col =c("annual" = "red","biennial"="green","triennial"="blue"), pch = c("annual"=1,"biennial"=2,"triennial"=3), cex = 0.5))
lines(x=ListDate,y=predNLS_lea_an,col = "red", lwd=1, lty = 1)
lines(x=ListDate,y=predNLS_lea_bi,col = "green", lwd=1, lty = 1)
lines(x=ListDate,y=predNLS_lea_tri,col = "blue", lwd=1, lty = 1)
legend("topright", legend = c("Annual", "Biennial", "Triennial",
                              "Annual Prediction", "Biennial Prediction","Triennial Prediction"),
       pch = c(1, 2, 3, NA , NA, NA), col = c("red", "green", "blue", "red", "green", "blue"),
       lty = c(NA , NA, NA, 1, 1, 1), title = "Fire Regime",cex = 0.5)

# Total Litterfall:

# Extraction des coefficients et des intervalles de confiance

conf_tot_an <- confint(NLS_tot_an)
conf_tot_bi <- confint(NLS_tot_bi)
conf_tot_tri <- confint(NLS_tot_tri)
conf_tot_ct_an <- confint(NLS_tot_ct_an)
conf_tot_ct_bi <- confint(NLS_tot_ct_bi)
conf_tot_ct_tri <- confint(NLS_tot_ct_tri)

# Création d'un nouveau dataframe avec les intervalles de confiance
coef_tot <- data.frame(
  Modele = c("NLS_tot_an", "NLS_tot_bi", "NLS_tot_tri",
             "NLS_tot_ct_an", "NLS_tot_ct_bi", "NLS_tot_ct_tri"),
  a = c(coef_tot_an["a"], coef_tot_bi["a"], coef_tot_tri["a"],
        coef_tot_ct_an["a"], coef_tot_ct_bi["a"], coef_tot_ct_tri["a"]),
  a_lower = c(conf_tot_an["a", 1], conf_tot_bi["a", 1], conf_tot_tri["a", 1],
              conf_tot_ct_an["a", 1], conf_tot_ct_bi["a", 1], conf_tot_ct_tri["a", 1]),
  a_upper = c(conf_tot_an["a", 2], conf_tot_bi["a", 2], conf_tot_tri["a", 2],
              conf_tot_ct_an["a", 2], conf_tot_ct_bi["a", 2], conf_tot_ct_tri["a", 2]),
  b = c(coef_tot_an["b"], coef_tot_bi["b"], coef_tot_tri["b"],
        coef_tot_ct_an["b"], coef_tot_ct_bi["b"], coef_tot_ct_tri["b"]),
  b_lower = c(conf_tot_an["b", 1], conf_tot_bi["b", 1], conf_tot_tri["b", 1],
              conf_tot_ct_an["b", 1], conf_tot_ct_bi["b", 1], conf_tot_ct_tri["b", 1]),
  b_upper = c(conf_tot_an["b", 2], conf_tot_bi["b", 2], conf_tot_tri["b", 2],
              conf_tot_ct_an["b", 2], conf_tot_ct_bi["b", 2], conf_tot_ct_tri["b", 2]),
  c = c(coef_tot_an["c"], coef_tot_bi["c"], coef_tot_tri["c"],
        coef_tot_ct_an["c"], coef_tot_ct_bi["c"], coef_tot_ct_tri["c"]),
  c_lower = c(conf_tot_an["c", 1], conf_tot_bi["c", 1], conf_tot_tri["c", 1],
              conf_tot_ct_an["c", 1], conf_tot_ct_bi["c", 1], conf_tot_ct_tri["c", 1]),
  c_upper = c(conf_tot_an["c", 2], conf_tot_bi["c", 2], conf_tot_tri["c", 2],
              conf_tot_ct_an["c", 2], conf_tot_ct_bi["c", 2], conf_tot_ct_tri["c", 2]),
  Base = c(coef_tot_an["Base"], coef_tot_bi["Base"], coef_tot_tri["Base"],
           coef_tot_ct_an["Base"], coef_tot_ct_bi["Base"], coef_tot_ct_tri["Base"]),
  Base_lower = c(conf_tot_an["Base", 1], conf_tot_bi["Base", 1], conf_tot_tri["Base", 1],
                 conf_tot_ct_an["Base", 1], conf_tot_ct_bi["Base", 1], conf_tot_ct_tri["Base", 1]),
  Base_upper = c(conf_tot_an["Base", 2], conf_tot_bi["Base", 2], conf_tot_tri["Base", 2],
                 conf_tot_ct_an["Base", 2], conf_tot_ct_bi["Base", 2], conf_tot_ct_tri["Base", 2]),
  p = c(coef_tot_an["p"], coef_tot_bi["p"], coef_tot_tri["p"],
        coef_tot_ct_an["p"], coef_tot_ct_bi["p"], coef_tot_ct_tri["p"]),
  p_lower = c(conf_tot_an["p", 1], conf_tot_bi["p", 1], conf_tot_tri["p", 1],
              conf_tot_ct_an["p", 1], conf_tot_ct_bi["p", 1], conf_tot_ct_tri["p", 1]),
  p_upper = c(conf_tot_an["p", 2], conf_tot_bi["p", 2], conf_tot_tri["p", 2],
              conf_tot_ct_an["p", 2], conf_tot_ct_bi["p", 2], conf_tot_ct_tri["p", 2]),
  A = c(coef_tot_an["A"], coef_tot_bi["A"], coef_tot_tri["A"],
        coef_tot_ct_an["A"], coef_tot_ct_bi["A"], coef_tot_ct_tri["A"]),
  A_lower = c(conf_tot_an["A", 1], conf_tot_bi["A", 1], conf_tot_tri["A", 1],
              conf_tot_ct_an["A", 1], conf_tot_ct_bi["A", 1], conf_tot_ct_tri["A", 1]),
  A_upper = c(conf_tot_an["A", 2], conf_tot_bi["A", 2], conf_tot_tri["A", 2],
              conf_tot_ct_an["A", 2], conf_tot_ct_bi["A", 2], conf_tot_ct_tri["A", 2])
)

# Affichage du dataframe
print(coef_tot)


# Leaves 

# Extraction des coefficients
coef_lea_an <- coef(NLS_lea_an)
coef_lea_bi <- coef(NLS_lea_bi)
coef_lea_tri <- coef(NLS_lea_tri)
coef_lea_ct_an <- coef(NLS_lea_ct_an)
coef_lea_ct_bi <- coef(NLS_lea_ct_bi)
coef_lea_ct_tri <- coef(NLS_lea_ct_tri)

# Création d'un nouveau dataframe
coef_lea <- data.frame(
  Modele = c("NLS_lea_an", "NLS_lea_bi", "NLS_lea_tri",
             "NLS_lea_ct_an", "NLS_lea_ct_bi", "NLS_lea_ct_tri"),
  a = c(coef_lea_an["a"], coef_lea_bi["a"], coef_lea_tri["a"],
        coef_lea_ct_an["a"], coef_lea_ct_bi["a"], coef_lea_ct_tri["a"]),
  b = c(coef_lea_an["b"], coef_lea_bi["b"], coef_lea_tri["b"],
        coef_lea_ct_an["b"], coef_lea_ct_bi["b"], coef_lea_ct_tri["b"]),
  c = c(coef_lea_an["c"], coef_lea_bi["c"], coef_lea_tri["c"]),
  Base = c(coef_lea_an["Base"], coef_lea_bi["Base"], coef_lea_tri["Base"],
           coef_lea_ct_an["Base"], coef_lea_ct_bi["Base"], coef_lea_ct_tri["Base"]),
  p = c(coef_lea_an["p"], coef_lea_bi["p"], coef_lea_tri["p"],
        coef_lea_ct_an["p"], coef_lea_ct_bi["p"], coef_lea_ct_tri["p"]),
  A = c(coef_lea_an["A"], coef_lea_bi["A"], coef_lea_tri["A"],
        coef_lea_ct_an["A"], coef_lea_ct_bi["A"], coef_lea_ct_tri["A"]))


# Twigs 

# Extraction des coeeficients : 
coef_twg_an <- coef(NLS_twg_an)
coef_twg_bi <- coef(NLS_twg_bi)
coef_twg_tri <- coef(NLS_twg_tri)
coef_twg_ct_an <- coef(NLS_twg_ct_an)
coef_twg_ct_bi <- coef(NLS_twg_ct_bi)
coef_twg_ct_tri <- coef(NLS_twg_ct_tri)

# Création d'un nouveau dataframe
coef_twg <- data.frame(
  Modele = c("NLS_twg_an", "NLS_twg_bi", "NLS_twg_tri",
             "NLS_twg_ct_an", "NLS_twg_ct_bi", "NLS_twg_ct_tri"),
  a = c(coef_twg_an["a"], coef_twg_bi["a"], coef_twg_tri["a"],
        coef_twg_ct_an["a"], coef_twg_ct_bi["a"], coef_twg_ct_tri["a"]),
  b = c(coef_twg_an["b"], coef_twg_bi["b"], coef_twg_tri["b"],
        coef_twg_ct_an["b"], coef_twg_ct_bi["b"], coef_twg_ct_tri["b"]),
  c = c(coef_twg_an["c"], coef_twg_bi["c"], coef_twg_tri["c"]),
  Base = c(coef_twg_an["Base"], coef_twg_bi["Base"], coef_twg_tri["Base"],
           coef_twg_ct_an["Base"], coef_twg_ct_bi["Base"], coef_twg_ct_tri["Base"]),
  p = c(coef_twg_an["p"], coef_twg_bi["p"], coef_twg_tri["p"],
        coef_twg_ct_an["p"], coef_twg_ct_bi["p"], coef_twg_ct_tri["p"]),
  A = c(coef_twg_an["A"], coef_twg_bi["A"], coef_twg_tri["A"],
        coef_twg_ct_an["A"], coef_twg_ct_bi["A"], coef_twg_ct_tri["A"]))

# Exportation des coefficients :
install.packages("openxlsx")
library(openxlsx)
write.xlsx(coef_tot, file = "coef_tot.xlsx")
write.xlsx(coef_lea, file = "coef_tot.xlsx")
write.xlsx(coef_twg, file = "coef_tot.xlsx")

coef_tot
coef_lea
coef_twg
