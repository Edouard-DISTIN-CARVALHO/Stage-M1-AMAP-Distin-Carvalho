~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#                          5/ Test Statistiques                                 ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git")
source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/4_Modelisation.R")

##### Paramètre des modèles #### 

# Regime de feu : 
dados_comp <- Mean %>%
  filter(fire_regime %in% c("annual","biennial","triennial"))
with(data=dados_comp, plot(x=date, y = total, main = "Fire regime Total Litterfall", col =c("annual" = "red","biennial"="green","triennial"="blue"), pch = c("annual"=1,"biennial"=2,"triennial"=3), cex = 0.5))
lines(x=ListDate,y=predNLS_tot_an,col = "red", lwd=1, lty = 1)
lines(x=ListDate,y=predNLS_tot_bi,col = "green", lwd=1, lty = 1)
lines(x=ListDate,y=predNLS_tot_tri,col = "blue", lwd=1, lty = 1)
legend("topright", legend = c("Annual", "Biennial", "Triennial",
                              "Annual Prediction", "Biennial Prediction","Triennial Prediction"),
       pch = c(1, 2, 3, NA , NA, NA), col = c("red", "green", "blue", "red", "green", "blue"),
       lty = c(NA , NA, NA, 1, 1, 1), title = "Fire Regime",cex = 0.5)

# Total Litterfall:

#Extraction des coefficients
coef_tot_an <- coef(NLS_tot_an)
coef_tot_bi <- coef(NLS_tot_bi)
coef_tot_tri <- coef(NLS_tot_tri)
coef_tot_ct_an <- coef(NLS_tot_ct_an)
coef_tot_ct_bi <- coef(NLS_tot_ct_bi)
coef_tot_ct_tri <- coef(NLS_tot_ct_tri)

# Création d'un nouveau dataframe
coef_tot_reg <- data.frame(
  Modele = c("NLS_tot_an", "NLS_tot_bi", "NLS_tot_tri",
            "NLS_tot_ct_an", "NLS_tot_ct_bi", "NLS_tot_ct_tri"),
  a = c(coef_tot_an["a"], coef_tot_bi["a"], coef_tot_tri["a"],
        coef_tot_ct_an["a"], coef_tot_ct_bi["a"], coef_tot_ct_tri["a"]),
  b = c(coef_tot_an["b"], coef_tot_bi["b"], coef_tot_tri["b"],
        coef_tot_ct_an["b"], coef_tot_ct_bi["b"], coef_tot_ct_tri["b"]),
  c = c(coef_tot_an["c"], coef_tot_bi["c"], coef_tot_tri["c"]),
  Base = c(coef_tot_an["Base"], coef_tot_bi["Base"], coef_tot_tri["Base"],
           coef_tot_ct_an["Base"], coef_tot_ct_bi["Base"], coef_tot_ct_tri["Base"]),
  p = c(coef_tot_an["p"], coef_tot_bi["p"], coef_tot_tri["p"],
        coef_tot_ct_an["p"], coef_tot_ct_bi["p"], coef_tot_ct_tri["p"]),
  A = c(coef_tot_an["A"], coef_tot_bi["A"], coef_tot_tri["A"],
        coef_tot_ct_an["A"], coef_tot_ct_bi["A"], coef_tot_ct_tri["A"]))

# Leaves 

# Extraction des coefficients
coef_lea_an <- coef(NLS_lea_an)
coef_lea_bi <- coef(NLS_lea_bi)
coef_lea_tri <- coef(NLS_lea_tri)
coef_lea_ct_an <- coef(NLS_lea_ct_an)
coef_lea_ct_bi <- coef(NLS_lea_ct_bi)
coef_lea_ct_tri <- coef(NLS_lea_ct_tri)

# Création d'un nouveau dataframe
coef_lea_reg <- data.frame(
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
coef_twg_reg <- data.frame(
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


