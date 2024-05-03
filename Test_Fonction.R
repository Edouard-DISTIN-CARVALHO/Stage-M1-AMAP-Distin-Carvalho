# Donnée réelle : 
ggplot(dados_norm, aes(x = date, y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_smooth(method="gam")  + 
  labs(title = "Evolution de la productivité primaire totale normalisée",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_m2)", 
       color = "Regime de feu") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + theme_classic() 

# Modèle : 

ListX<-seq(0,6,length=1001)

myFuncA<-function(x, a= 6, b = 0.5, c = 1.2)
{ return(exp(-b*x)*sin(a*x-c)+ b*c) }
myFuncB <- function(x, a = 3, b = 0.5, c = 1.2) 
{ return(exp(-b*x)*sin(a*x-c)+ b*c) }

plot(x = ListX , y = myFuncA(x=ListX, a = 6, b = 0.5, c = 1.2), 
     type="l", col="green", main = "Modèle théorique", 
     xlab = "Temps", ylab = "NPP",)
lines(x = ListX, y = myFuncB(x = ListX, a = 3, b = 0.5, c = 1.2), 
      type = "l", col = "red")
legend("topright", legend = c("Courbe d'effet du feu 2018-2021", "Courbe d'effet du feu 2021-2023"), 
       col = c("green","red"), lty = 1, cex = 0.8)

