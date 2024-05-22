#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#                          4/ Modélisation                                      ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getwd()
#setwd("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git") # AMAP
setwd("C:/Users/edoua/OneDrive/Documents/R/AMAP/Git") # Galaxybook
source("3_ Saisonalite.R")

library(dplyr)
library(ggplot2)

##### Fonction modèle ###### 

# Modèle :
myFunc<-function(x, a= 6, b = 0.5, c = 1.2,Base=15,p=1, A=1)
{ return( Base+A*exp(-b*x)*(sin(2*pi*(x-c)/a)+1)^p) }# Base : paramètre d'échelle verticale  
# a : période d'oscillations 
# b : coefficient d'écrasement des oscillations 
# c : paramètre de positions des oscillations sur l'axe des x 
# p : paramètre d'écrasement des oscillations 
# A : paramètre d'amplitude 

# Mise en format date de x 
ListDate<-seq.Date(from=min(Mean$date,na.rm=TRUE),to=max(Mean$date,na.rm=TRUE),by=1)

## Total :
with(data=Mean,plot(x=date,y=total, main = "Totall_Litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0001,
                          c = 1,
                          Base=1,
                          p=3, A =1 ),col="red")


## Leaves
with(data=Mean,plot(x=date,y=leaves,main = "Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0004,
                          c = 110,
                          Base=0.5,
                          p=3, A = 1.5),col="darkgreen")

## Twigs 
with(data=Mean,plot(x=date,y=twigs, main = "Twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0006,
                          c = 120,
                          Base=.1,
                          p=2, A = 0.3),col="brown")

#### Etudes de l'effet des régimes de feu sur la productivité primaire totale ####

###### Annual ######
dados_an <- Mean[Mean$fire_regime == "annual",]
dados_an$day<-as.numeric(dados_an$date-min(dados_an$date,na.rm=TRUE))

# Total_litterfall 
with(data=dados_an,plot(x=date,y=total,main = "Annual Total_litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0005,
                          c = 110,
                          Base=1,
                          p=3, 
                          A = 1),col="red", lty="dashed")

NLS_tot_an<-nls(data=dados_an, formula = 
           total~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
           start=
             c(a = 365, 
             b = .0005,
             c = 110,
             Base=1,
             p=3, 
             A = 1), control=list(maxiter=5000))
predNLS_tot_an<-predict(NLS_tot_an,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_tot_an,col="cyan")

# Leaves 
with(data=dados_an,plot(x=date,y=leaves,main = "Annual Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                             a = 365, 
                             b = 0.0001,
                             c = 120,
                             Base=.5,
                             p=3, 
                             A = 0.8),col="darkgreen", lty="dashed")

dados_an$day<-as.numeric(dados_an$date-min(dados_an$date,na.rm=TRUE))
NLS_lea_an<-nls(data=dados_an, formula = 
                  leaves~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                start=c(a = 365, 
                        b = 0.0001,
                        c = 120,
                        Base=.5,
                        p=3, 
                        A = 0.8), control=list(maxiter=5000))
predNLS_lea_an<-predict(NLS_lea_an,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_lea_an,col="#FF007F")

# Twigs 
with(data=dados_an,plot(x=date,y=twigs,main = "Annual twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                             a = 365, 
                             b = .0012,
                             c = 140,
                             Base=0.1,
                             p=2, 
                             A = .7),col="brown", lty="dashed")

dados_an$day<-as.numeric(dados_an$date-min(dados_an$date,na.rm=TRUE))
NLS_twg_an<-nls(data=dados_an, formula = 
                  twigs~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                start=c(a = 365, 
                        b = .0012,
                        c = 140,
                        Base=0.1,
                        p=2, 
                        A = .7), control=list(maxiter=5000))
predNLS_twg_an<-predict(NLS_twg_an,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_twg_an,col="cyan")

###### Biannual #####
dados_bi <- Mean[Mean$fire_regime == "biennial",]

# Total_litterfall 
with(data=dados_bi,plot(x=date,y=total,main = "Biennial Total_litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0006,
                          c = 100,
                          Base=.5,
                          p=4,
                          A= .5),
                          col="red", lty="dashed")

dados_bi$day<-as.numeric(dados_bi$date-min(dados_bi$date,na.rm=TRUE))
NLS_tot_bi<-nls(data=dados_bi, formula = 
                  total~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                start=c(a = 365, 
                        b = .0006,
                        c = 100,
                        Base=.5,
                        p=4,
                        A= .5), control=list(maxiter=5000))
predNLS_tot_bi<-predict(NLS_tot_bi,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_tot_bi,col="cyan")

# Leaves 
with(data=dados_bi,plot(x=date,y=leaves,main = "Biennial Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0005,
                          c = 100,
                          Base=.3,
                          p=3, 
                          A = 1),col="darkgreen", lty="dashed")

dados_bi$day<-as.numeric(dados_bi$date-min(dados_bi$date,na.rm=TRUE))
NLS_lea_bi<-nls(data=dados_bi, formula = 
                  leaves~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                start=c(a = 365, 
                        b = .0005,
                        c = 100,
                        Base=.3,
                        p=3, 
                        A = 1), 
                control = nls.control(minFactor = 1e-10, maxiter = 5000))
predNLS_lea_bi<-predict(NLS_lea_bi,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_lea_bi,col="#FF007F")

# Twigs 
with(data=dados_bi,plot(x=date,y=twigs,main = "Biennial twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0001,
                          c = 120,
                          Base=.05,
                          p=3, 
                          A = 0.1),col="brown", lty ="dashed")

dados_bi$day<-as.numeric(dados_bi$date-min(dados_bi$date,na.rm=TRUE))
NLS_twg_bi<-nls(data=dados_bi, formula = 
                  twigs~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                start=c(a = 365, 
                        b = .0001,
                        c = 120,
                        Base=.05,
                        p=3, 
                        A = 0.1), 
                control = nls.control(minFactor = 1e-10, maxiter = 10000))
predNLS_twg_bi<-predict(NLS_twg_bi,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_twg_bi,col="cyan")


###### Triannial #####
dados_tri <- Mean[Mean$fire_regime == "triennial",]

# Total_litterfall 
with(data=dados_tri,plot(x=date,y=total,main = "Triennial Total_litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .001,
                          c = 120,
                          Base=.1,
                          p=3, 
                          A=1),
      col="red", lty="dashed")

dados_tri$day<-as.numeric(dados_tri$date-min(dados_tri$date,na.rm=TRUE))
NLS_tot_tri<-nls(data=dados_tri, formula = 
                   total~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                 start=c(a = 365, 
                         b = .001,
                         c = 120,
                         Base=.1,
                         p=3, 
                         A=1), control=list(maxiter=5000))
predNLS_tot_tri<-predict(NLS_tot_tri,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_tot_tri,col="cyan")

# Leaves 
with(data=dados_tri,plot(x=date,y=leaves,main = "Triennial Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .001,
                          c = 100,
                          Base=.2,
                          p=3, 
                          A = 1),col="darkgreen", lty="dashed")

dados_tri$day<-as.numeric(dados_tri$date-min(dados_tri$date,na.rm=TRUE))
NLS_lea_tri<-nls(data=dados_tri, formula = 
                   leaves~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                 start=c(a = 365, 
                         b = .001,
                         c = 100,
                         Base=.2,
                         p=3, A = 1), control=list(maxiter=5000))
predNLS_lea_tri<-predict(NLS_lea_tri,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_lea_tri,col="#FF007F")

# Twigs 
with(data=dados_tri,plot(x=date,y=twigs,main = "Triennial twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0001,
                          c = 100,
                          Base=0.01,
                          p=3, 
                          A = 0.1),col="brown", lty ="dashed")

dados_tri$day<-as.numeric(dados_tri$date-min(dados_tri$date,na.rm=TRUE))
NLS_twg_tri<-nls(data=dados_tri, formula = 
                   twigs~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                 start=c(a = 360, 
                         b = .0001,
                         c = 100,
                         Base=0.01,
                         p=3, 
                         A = 0.1), control=list(maxiter=5000))
predNLS_twg_tri<-predict(NLS_twg_tri,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_twg_tri,col="cyan")

###### Annual Control ######
dados_ct_an <- Mean[Mean$fire_regime == "control_an",]
dados_ct_an$day<-as.numeric(dados_ct_an$date-min(dados_ct_an$date,na.rm=TRUE))

# Total_litterfall 
with(data=dados_ct_an,plot(x=date,y=total,main = "control_an Total_litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = 0,
                          c = 100,
                          Base=1,
                          p=1.8, 
                          A = 2),col="red", lty="dashed")

NLS_tot_ct_an<-nls(data=dados_ct_an, formula = 
                     total~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                   start=c(a = 365, 
                           b = 0,
                           c = 100,
                           Base=1,
                           p=1.8, 
                           A = 2), control=list(maxiter=5000))
predNLS_tot_ct_an<-predict(NLS_tot_ct_an,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_tot_ct_an,col="cyan")

# Leaves 
with(data=dados_ct_an,plot(x=date,y=leaves,main = "control_an Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = 0,
                          c = 100,
                          Base=0.5,
                          p=3, 
                          A = 1),col="darkgreen", lty="dashed")

dados_ct_an$day<-as.numeric(dados_ct_an$date-min(dados_ct_an$date,na.rm=TRUE))
NLS_lea_ct_an<-nls(data=dados_ct_an, formula = 
                     leaves~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                   start=c(a = 365, 
                           b = 0,
                           c = 100,
                           Base=0.5,
                           p=3, 
                           A = 1), control=list(maxiter=5000))
predNLS_lea_ct_an<-predict(NLS_lea_ct_an,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_lea_ct_an,col="#FF007F")

# Twigs 
with(data=dados_ct_an,plot(x=date,y=twigs,main = "control_an twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = 0,
                          c = 100,
                          Base=0.1,
                          p=2, 
                          A = .5),col="brown", lty="dashed")

dados_ct_an$day<-as.numeric(dados_ct_an$date-min(dados_ct_an$date,na.rm=TRUE))
NLS_twg_ct_an<-nls(data=dados_ct_an, formula = 
                     twigs~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                   start=c(a = 365, 
                           b = 0,
                           c = 100,
                           Base=0.1,
                           p=2, 
                           A = .5), control=list(maxiter=5000))
predNLS_twg_ct_an<-predict(NLS_twg_ct_an,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_twg_ct_an,col="cyan")

###### Biennial control #####
dados_ct_bi <- Mean[Mean$fire_regime == "control_bi",]

# Total_litterfall 
with(data=dados_ct_bi,plot(x=date,y=total,main = "control_bi Total_litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = 0,
                          c = 100,
                          Base=.5,
                          p=3, 
                          A= 1),
      col="green", lty="dashed")

dados_ct_bi$day<-as.numeric(dados_ct_bi$date-min(dados_ct_bi$date,na.rm=TRUE))
NLS_tot_ct_bi<-nls(data=dados_ct_bi, formula = 
                     total~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                   start=c(a = 365, 
                           b = 0,
                           c = 100,
                           Base=.5,
                           p=3, 
                           A= 1), control=list(maxiter=5000))
predNLS_tot_ct_bi<-predict(NLS_tot_ct_bi,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_tot_ct_bi,col="cyan")

# Leaves 
with(data=dados_ct_bi,plot(x=date,y=leaves,main = "control_bi Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = 0,
                          c = 100,
                          Base=.5,
                          p=3, 
                          A = 1),col="darkgreen", lty="dashed")

dados_ct_bi$day<-as.numeric(dados_ct_bi$date-min(dados_ct_bi$date,na.rm=TRUE))
NLS_lea_ct_bi<-nls(data=dados_ct_bi, formula = 
                     leaves~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                   start=c(a = 365, 
                           b = 0,
                           c = 100,
                           Base=.5,
                           p=3, 
                           A = 1), control=list(maxiter=5000))
predNLS_lea_ct_bi<-predict(NLS_lea_ct_bi,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_lea_ct_bi,col="#FF007F")

# Twigs 
with(data=dados_ct_bi,plot(x=date,y=twigs,main = "control_bi twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = 0,
                          c = 100,
                          Base=.1,
                          p=2, 
                          A = .1),col="brown", lty ="dashed")

dados_ct_bi$day<-as.numeric(dados_ct_bi$date-min(dados_ct_bi$date,na.rm=TRUE))
NLS_twg_ct_bi<-nls(data=dados_ct_bi, formula = 
                     twigs~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                   start=c(a = 365, 
                           b = 0,
                           c = 100,
                           Base=.1,
                           p=2, 
                           A = .1), control=list(maxiter=5000))
predNLS_twg_ct_bi<-predict(NLS_twg_ct_bi,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_twg_ct_bi,col="cyan")


###### Triennial Control #####
dados_ct_tri <- Mean[Mean$fire_regime == "control_tri",]

# Total_litterfall 
with(data=dados_ct_tri,plot(x=date,y=total,main = "control_tri Total_litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = 0,
                          c = 100,
                          Base=.5,
                          p=2, 
                          A=2),
      col="red", lty="dashed")

dados_ct_tri$day<-as.numeric(dados_ct_tri$date-min(dados_ct_tri$date,na.rm=TRUE))
NLS_tot_ct_tri<-nls(data=dados_ct_tri, formula = 
                      total~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                    start=c(a = 365, 
                            b = 0,
                            c = 100,
                            Base=.5,
                            p=2, 
                            A=2), control=list(maxiter=5000))
predNLS_tot_ct_tri<-predict(NLS_tot_ct_tri,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_tot_ct_tri,col="cyan")

# Leaves 
with(data=dados_ct_tri,plot(x=date,y=leaves,main = "control_tri Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = 0,
                          c = 100,
                          Base=.4,
                          p=3, 
                          A = .7),col="darkgreen", lty="dashed")

dados_ct_tri$day<-as.numeric(dados_ct_tri$date-min(dados_ct_tri$date,na.rm=TRUE))
NLS_lea_ct_tri<-nls(data=dados_ct_tri, formula = 
                      leaves~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                    start=c( a = 365, 
                             b = 0,
                             c = 100,
                             Base=.4,
                             p=3, 
                             A = .7), control=list(maxiter=5000))
predNLS_lea_ct_tri<-predict(NLS_lea_ct_tri,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_lea_ct_tri,col="#FF007F")

# Twigs 
with(data=dados_ct_tri,plot(x=date,y=twigs,main = "control_tri twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 400, 
                          b = 0,
                          c = 100,
                          Base=0.01,
                          p=2, 
                          A = 0.1),col="brown", lty ="dashed")

dados_ct_tri$day<-as.numeric(dados_ct_tri$date-min(dados_ct_tri$date,na.rm=TRUE))
NLS_twg_ct_tri<-nls(data=dados_ct_tri, formula = 
                      twigs~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p, A=A),
                    start=c(a = 400, 
                            b = 0,
                            c = 100,
                            Base=0.01,
                            p=2, 
                            A = 0.1), control=list(maxiter=5000))
predNLS_twg_ct_tri<-predict(NLS_twg_ct_tri,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_twg_ct_tri,col="cyan")

###### Control #####
dados_ct_comp <- Mean %>%
  filter(fire_regime %in% c("control_an","control_bi","control_tri"))
with(data=dados_ct_comp, plot(x=date, y = total, main = "Fire regime Control Total Litterfall", col =c("control_an" = "red","control_bi"="green","control_tri"="blue"), pch = c("control_an"=1,"control_bi"=2,"control_tri"=3), cex = 0.5))
lines(x=ListDate,y=predNLS_tot_ct_an,col = "red", lwd=1, lty = 2)
lines(x=ListDate,y=predNLS_tot_ct_bi,col = "green", lwd=1, lty = 2)
lines(x=ListDate,y=predNLS_tot_ct_tri,col = "blue", lwd=1, lty = 2)
legend("topright", legend = c("Control_an", "Control_bi", "Control_tri",
                              "Control_an Prediction", "Control_bi Prediction","Control_tri Prediction"),
       pch = c(1, 2, 3, NA , NA, NA), col = c("red", "green", "blue", "red", "green", "blue"),
       lty = c(NA , NA, NA, 2, 2, 2), title = "Fire Regime",cex = 0.5)

###### Fire Regime #####
dados_comp <- Mean %>%
  filter(fire_regime %in% c("annual","biennial","triennial"))
with(data=dados_comp, plot(x=date, y = total, main = "Fire regime Total Litterfall", col =c("annual" = "red","biennial"="green","triennial"="blue"), pch = c("annual"=1,"biennial"=2,"triennial"=3), cex = 0.5))
lines(x=ListDate,y=predNLS_tot_an,col = "red", lwd=1, lty = 1)
lines(x=ListDate,y=predNLS_tot_bi,col = "green", lwd=1, lty = 1)
lines(x=ListDate,y=predNLS_tot_tri,col = "blue", lwd=1, lty = 1)
legend("topright", legend = c("Annual", "Biennial", "Triennial",
                              "Annual Prediction", "Biennial Prediction","Triennial Prediction"),
       pch = c(1, 2, 3, NA , NA, NA), col = c("red", "green", "blue", "red", "green", "blue"),
       lty = c(NA , NA, NA, 1, 1, 1), title = "Fire Regime",cex = 0.4)


#### Comparaison entre modèle fire_regim et control ####

###### Total Litterfall #####

### Annual vs Control 
dados_comp_an <- Mean %>%
  filter(fire_regime %in% c("annual","control_an"))

with(data=dados_comp_an,plot(x=date,y=total,
                             main = "Annual vs Control Total_litterfall",
                             col = c("annual"="red","control_an"= "black"),
                             pch = c("annual"= 17, "control_an" = 0) , cex = 0.6)) 
lines(x=ListDate,y=predNLS_tot_an,col = "red", lwd=1)
lines(x=ListDate,y=predNLS_tot_ct_an, col = "black",lwd=1,lty=2)
legend("topright", legend = c("Annual", "Control_an", "Annual Prediction", "Control_an Prediction"), 
       pch = c(17, 0, NA, NA), col = c("red", "black", "red", "black"),
       lty = c(NA, NA, 1, 2), title = "Fire Regime",
       cex = 0.5)

##### Biennial vs Control
dados_comp_bi <- Mean %>%
  filter(fire_regime %in% c("biennial","control_bi"))

with(data=dados_comp_bi,plot(x=date,y=total,
                             main = "Biennial vs Control Total_litterfall",
                             col = c("biennial"="green","control_bi"= "black"),
                             pch =c("biennial" = 17, "control_bi"=0), cex = .6)) 
lines(x=ListDate,y=predNLS_tot_bi,col = "green",lwd=1)
lines(x=ListDate,y=predNLS_tot_ct_bi, col = "black", lty= 2, lwd = 1)
legend("topright", legend = c("Biennial", "Control_bi", "Biennial Prediction", "Control_bi Prediction"), 
       pch = c(17, 0, NA, NA), col = c("green", "black", "green", "black"),
       lty = c(NA, NA, 1, 2), title = "Fire Regime",
       cex = 0.5)

#### Triennial vs Control
dados_comp_tri <- Mean %>%
  filter(fire_regime %in% c("triennial","control_tri"))

with(data=dados_comp_tri,plot(x=date,y=total,
                              main = "Triennial vs Control Total_litterfall",
                              col = c("triennial"="blue","control_tri"= "black"),
                              pch = c("triennial"=17,"control_tri"= 0), cex = .6)) 
lines(x=ListDate,y=predNLS_tot_tri,col = "blue",lwd=1)
lines(x=ListDate,y=predNLS_tot_ct_tri, col = "black", lty = 2, lwd = 1)
legend("topright", legend = c("Triennial", "Control_tri", "Triennial Prediction", "Control_tri Prediction"), 
       pch = c(17, 0, NA, NA), col = c("blue", "black", "blue", "black"),
       lty = c(NA, NA, 1, 2), title = "Fire Regime",
       cex = 0.6)

###### Leaves ####

#### Annual vs Control
with(data=dados_comp_an,plot(x=date,y=leaves,
                             main = "Annual vs Control Leaves",
                             col = c("annual"="red","control_an"= "black"),
                             pch =  c("annual"= 18, "control_an" = 0), cex = 0.6)) 
lines(x=ListDate,y=predNLS_lea_an,col = "red", lwd=1)
lines(x=ListDate,y=predNLS_lea_ct_an, col = "black",lwd=1,lty=2)
legend("topright", legend = c("Annual", "Control_an", "Annual Prediction", "Control_an Prediction"), 
       pch = c(18, 0, NA, NA), col = c("red","black", "red", "black"),
       lty = c(NA, NA, 1, 2), title = "Fire Regime",
       cex = 0.5)

#### Biennial vs Control
with(data=dados_comp_bi,plot(x=date,y=leaves,
                             main = "Biennial vs Control Leaves",
                             col = c("biennial"="green","control_bi"= "black"),
                             pch =c("biennial" = 18, "control_bi"=0), cex = .6)) 
lines(x=ListDate,y=predNLS_lea_bi,col = "green",lwd=1)
lines(x=ListDate,y=predNLS_lea_ct_bi, col = "black", lty = 2, lwd = 1)
legend("topright", legend = c("Biennial", "Control_bi", "biennial Prediction", "Control_bi Prediction"), 
       pch = c(18, 0, NA, NA), col = c("green", "black", "green", "black"),
       lty = c(NA, NA, 1, 2), title = "Fire Regime",
       cex = 0.6)

#### Triennial vs Control
with(data=dados_comp_tri,plot(x=date,y=leaves,
                              main = "Triennial vs Control Leaves",
                              col = c("triennial"="blue","control_tri"= "black"),
                              pch =c("triennial" = 18, "control_tri"=0), cex = .6)) 
lines(x=ListDate,y=predNLS_lea_tri,col = "blue",lwd=1)
lines(x=ListDate,y=predNLS_lea_ct_tri, col = "black", lty = 2, lwd = 1)
legend("topright", legend = c("Triennial", "Control_tri", "triennial Prediction", "Control_tri Prediction"), 
       pch = c(18, 0, NA, NA), col = c("blue", "black", "blue", "black"),
       lty = c(NA, NA, 1, 2), title = "Fire Regime",
       cex = 0.6)

###### Twigs ####

#### Annual vs Control
with(data=dados_comp_an,plot(x=date,y=twigs,
                             main = "Annual vs Control Twigs",
                             col = c("annual"="red","control_an"= "black"),
                             pch = c("annual" = 10, "control_an"=0), cex = 0.6)) 
lines(x=ListDate,y=predNLS_twg_an,col = "red", lwd=1)
lines(x=ListDate,y=predNLS_twg_ct_an, col = "black",lwd=1,lty=2)
legend("topright", legend = c("Annual", "Control_an", "Annual Prediction", "Control_an Prediction"), 
       pch = c(10, 0, NA, NA), col = c("red", "black", "red", "black"),
       lty = c(NA, NA, 1, 2), title = "Fire Regime",
       cex = 0.5)

#### Biennial vs Control
with(data=dados_comp_bi,plot(x=date,y=twigs,
                             main = "Biennial vs Control Twigs",
                             col = c("biennial"="green","control_bi"= "black"),
                             pch =c("biennial" = 10, "control_bi"=0), cex = .6)) 
lines(x=ListDate,y=predNLS_twg_bi,col = "green",lwd=1)
lines(x=ListDate,y=predNLS_twg_ct_bi, col = "black", lty = 2, lwd = 1)
legend("topright", legend = c("Biennial", "Control_bi", "Biennial Prediction", "Control_bi Prediction"), 
       pch = c(10, 0, NA, NA), col = c("green", "black", "green", "black"),
       lty = c(NA, NA, 1, 2), title = "Fire Regime",
       cex = 0.6)

#### Triennial vs Control
with(data=dados_comp_tri,plot(x=date,y=twigs,
                              main = "Triennial vs Control Twigs",
                              col = c("triennial"="blue","control_tri"= "black"),
                              pch =c("triennial" = 10, "control_tri"=0), cex = .6)) 
lines(x=ListDate,y=predNLS_twg_tri,col = "blue",lwd=1)
lines(x=ListDate,y=predNLS_twg_ct_tri, col = "black", lty = 2, lwd = 1)
legend("topright", legend = c("Triennial", "Control_tri", "Triennial Prediction", "Control_tri Prediction"), 
       pch = c(10, 0, NA, NA), col = c("blue", "black", "blue", "black"),
       lty = c(NA, NA, 1, 2), title = "Fire Regime",
       cex = 0.6)
