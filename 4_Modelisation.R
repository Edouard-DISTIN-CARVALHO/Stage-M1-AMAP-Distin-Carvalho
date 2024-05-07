#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#                          4/ Modélisation                                      ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/1_Standardisation.R") 
source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/2_Composition.R") 
source("C:/Users/distincarvalho/OneDrive/Documents/R/AMAP/Git/3_Saisonalite.R") 

# Library : 

library(dplyr)
library(ggplot2)

##### Fonction modèle ###### 

# Modèle :
myFunc<-function(x, a= 6, b = 0.5, c = 1.2,Base=25,p=1)
{ return( Base*exp(-b*x)*(sin(2*pi*(x-c)/a)+1)^p) }

# Base : paramètre d'échelle verticale  
# a : période d'oscillations 
# b : coefficient d'écrasement des oscillations 
# c : paramètre de positions des oscillations sur l'axe des x 
# p : paramètre d'écrasement des oscillations 

# Mise en forme des donnnées réelles : 

dados_mod <- dados %>%  group_by(plot_code, date) %>%
  summarize(total_MgC_m2 = mean(total_litterfall_MgC_ha_year, na.rm = TRUE), 
            leaves = mean(leaves_MgC_ha_year, na.rm = TRUE), 
            twigs = mean(twigs_MgC_ha_year, na.rm = TRUE))
dados_mod  <- dados_mod  %>%  mutate(fire_regime = plot_code)
names(dados_mod )[names(dados_mod ) == "dados_mod$fire_regime"] <- "fire_regime"
dados_mod$fire_regime <- factor(dados_mod$fire_regime, 
                                levels = c("ESA-04", "ESA-05", "ESA-06", "ESA-07", "ESA-08", "ESA-09"),
                                labels = c("control_bi", "biennial", "control_tri", "triennial", "control_an", "annual"))
dados_mod$date <- as.Date(dados_mod$date, format = "%d/%m/%Y")
dados_mod <- dados_mod %>% arrange(date)

# Mise en format date de x 
ListDate<-seq.Date(from=min(dados_mod$date,na.rm=TRUE),to=max(dados_mod$date,na.rm=TRUE),by=1)

## Total :
with(data=dados_mod,plot(x=date,y=total_MgC_m2, main = "Totall_Litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0005,
                          c = 120,
                          Base=2,
                          p=3),col="red")


## Leaves
with(data=dados_mod,plot(x=date,y=leaves,main = "Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0004,
                          c = 110,
                          Base=1.5,
                          p=3),col="darkgreen")

## Twigs 
with(data=dados_mod,plot(x=date,y=twigs, main = "Twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0006,
                          c = 120,
                          Base=.5,
                          p=3),col="brown")

#### Etudes de l'effet des régimes de feu sur la productivité primaire totale ####

### ANNUAL #####
dados_an <- dados_mod[dados_mod$fire_regime == "annual",]
dados_an$day<-as.numeric(dados_an$date-min(dados_an$date,na.rm=TRUE))

# Total_litterfall 
with(data=dados_an,plot(x=date,y=total_MgC_m2,main = "Annual Total_litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0001,
                          c = 110,
                          Base=2,
                          p=2.5),col="red", lty="dashed")

NLS_tot_an<-nls(data=dados_an, formula = 
           total_MgC_m2~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p),
     start=c(a = 365, b = .0001, c = 110, Base=2, p=2.5), control=list(maxiter=5000))
predNLS_tot_an<-predict(NLS_tot_an,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_tot_an,col="cyan")

# Leaves 
with(data=dados_an,plot(x=date,y=leaves,main = "Annual Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                             a = 365, 
                             b = .0006,
                             c = 120,
                             Base=2,
                             p=2.5),col="darkgreen", lty="dashed")

dados_an$day<-as.numeric(dados_an$date-min(dados_an$date,na.rm=TRUE))
NLS_lea_an<-nls(data=dados_an, formula = 
                  leaves~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p),
                start=c(a = 365, b = .0006, c = 120, Base=2, p=2.5), control=list(maxiter=5000))
predNLS_lea_an<-predict(NLS_lea_an,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_lea_an,col="#FF007F")

# Twigs 
with(data=dados_an,plot(x=date,y=twigs,main = "Annual twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                             a = 365, 
                             b = .0005,
                             c = 140,
                             Base=0.3,
                             p=2.2),col="brown", lty="dashed")

dados_an$day<-as.numeric(dados_an$date-min(dados_an$date,na.rm=TRUE))
NLS_twg_an<-nls(data=dados_an, formula = 
                  twigs~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p),
                start=c(a = 365, b = .0005, c = 140, Base=3, p=2.2), control=list(maxiter=5000))
predNLS_twg_an<-predict(NLS_twg_an,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_twg_an,col="cyan")

### Biannual #####
dados_bi <- dados_mod[dados_mod$fire_regime == "biennial",]

# Total_litterfall 
with(data=dados_bi,plot(x=date,y=total_MgC_m2,main = "Biennial Total_litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 370, 
                          b = .0006,
                          c = 100,
                          Base=.5,
                          p=4),
                          col="red", lty="dashed")

dados_bi$day<-as.numeric(dados_bi$date-min(dados_bi$date,na.rm=TRUE))
NLS_tot_bi<-nls(data=dados_bi, formula = 
                  total_MgC_m2~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p),
                start=c(a = 370, b = .0006, c = 100, Base=5, p=4), control=list(maxiter=5000))
predNLS_tot_bi<-predict(NLS_tot_bi,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_tot_bi,col="cyan")

# Leaves 
with(data=dados_bi,plot(x=date,y=leaves,main = "Biennial Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0008,
                          c = 100,
                          Base=.5,
                          p=4),col="darkgreen", lty="dashed")

dados_bi$day<-as.numeric(dados_bi$date-min(dados_bi$date,na.rm=TRUE))
NLS_lea_bi<-nls(data=dados_bi, formula = 
                  leaves~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p),
                start=c(a = 365, b = .0008, c = 100, Base=5, p=4), control=list(maxiter=5000))
predNLS_lea_bi<-predict(NLS_lea_bi,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_lea_bi,col="#FF007F")

# Twigs 
with(data=dados_bi,plot(x=date,y=twigs,main = "Biennial twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 400, 
                          b = .0001,
                          c = 100,
                          Base=.1,
                          p=2),col="brown", lty ="dashed")

dados_bi$day<-as.numeric(dados_bi$date-min(dados_bi$date,na.rm=TRUE))
NLS_twg_bi<-nls(data=dados_bi, formula = 
                  twigs~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p),
                start=c(a = 400, b = .0001, c = 100, Base=1, p=2), control=list(maxiter=5000))
predNLS_twg_bi<-predict(NLS_twg_bi,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_twg_bi,col="cyan")


### Triannial #####
dados_tri <- dados_mod[dados_mod$fire_regime == "triennial",]

# Total_litterfall 
with(data=dados_tri,plot(x=date,y=total_MgC_m2,main = "Triennial Total_litterfall"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 380, 
                          b = .0003,
                          c = 90,
                          Base=.5,
                          p=3.6),
      col="red", lty="dashed")

dados_tri$day<-as.numeric(dados_tri$date-min(dados_tri$date,na.rm=TRUE))
NLS_tot_tri<-nls(data=dados_tri, formula = 
                   total_MgC_m2~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p),
                 start=c(a = 380, b = .0003, c = 90, Base=3.2, p=3.6), control=list(maxiter=5000))
predNLS_tot_tri<-predict(NLS_tot_tri,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_tot_tri,col="cyan")

# Leaves 
with(data=dados_tri,plot(x=date,y=leaves,main = "Triennial Leaves"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, 
                          b = .0004,
                          c = 100,
                          Base=.2,
                          p=4),col="darkgreen", lty="dashed")

dados_tri$day<-as.numeric(dados_tri$date-min(dados_tri$date,na.rm=TRUE))
NLS_lea_tri<-nls(data=dados_tri, formula = 
                   leaves~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p),
                 start=c(a = 365, b = .0004, c = 100, Base=2, p=4), control=list(maxiter=5000))
predNLS_lea_tri<-predict(NLS_lea_tri,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_lea_tri,col="#FF007F")

# Twigs 
with(data=dados_tri,plot(x=date,y=twigs,main = "Triennial twigs"))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 400, 
                          b = .0001,
                          c = 70,
                          Base=0.05,
                          p=3),col="brown", lty ="dashed")

dados_tri$day<-as.numeric(dados_tri$date-min(dados_tri$date,na.rm=TRUE))
NLS_twg_tri<-nls(data=dados_tri, formula = 
                   twigs~myFunc(x=day, a = a, b = b, c = c, Base=Base, p=p),
                 start=c(a = 400, b = .0001, c = 70, Base=0.5, p=3), control=list(maxiter=5000))
predNLS_twg_tri<-predict(NLS_twg_tri,newdata=data.frame(day=as.numeric(ListDate-min(ListDate))))
lines(x=ListDate,y=predNLS_twg_tri,col="cyan")


