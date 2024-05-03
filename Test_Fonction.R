# Donnée réelle : 
dados_an <- dados[dados$fire_regime == "annual",]
ggplot(dados_an, aes(x = date, log= "y", y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_point()  + 
  labs(title = "Evolution de la productivité primaire totale",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_m2)", 
       color = "Regime de feu") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + theme_classic() 

with(data=dados_an,plot(x=date,y=total_litterfall_MgC_ha_year,log="y"))

dados_bi <- dados[dados$fire_regime == "biennial",]
ggplot(dados_bi, aes(x = date, y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_point()  + 
  labs(title = "Evolution de la productivité primaire totale",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_m2)", 
       color = "Regime de feu") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + theme_classic() 

dados_tri <- dados[dados$fire_regime == "triennial",]
ggplot(dados_tri, aes(x = date, y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_point()  + 
  labs(title = "Evolution de la productivité primaire totale",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_m2)", 
       color = "Regime de feu") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + theme_classic()

dados_ctan <- dados[dados$fire_regime == "control_an",]
ggplot(dados_ctan, aes(x = date, y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_point()  + 
  labs(title = "Evolution de la productivité primaire totale",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_m2)", 
       color = "Regime de feu") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + theme_classic()

dados_ctbi <- dados[dados$fire_regime == "control_bi",]
ggplot(dados_ctbi, aes(x = date, y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_point()  + 
  labs(title = "Evolution de la productivité primaire totale",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_m2)", 
       color = "Regime de feu") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + theme_classic()

dados_ctri<- dados[dados$fire_regime == "control_tri",]
ggplot(dados_ctri, aes(x = date, y = total_litterfall_MgC_ha_year, color=fire_regime)) +
  geom_point()  + 
  labs(title = "Evolution de la productivité primaire totale",    
       x = "Date de collecte", y = "Productivité primaire totale (MgC_m2)", 
       color = "Regime de feu") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + theme_classic()



# Modèle : 

ListX<-seq(0,6,length=1001)

myFunc<-function(x, a= 6, b = 0.5, c = 1.2,Base=25,p=1)
{ return( Base*exp(-b*x)*(sin(2*pi*(x-c)/a)+1)^p) }

ListDate<-seq.Date(from=min(dados$date,na.rm=TRUE),to=max(dados$date,na.rm=TRUE),by=1)
with(data=dados_an,plot(x=date,y=total_litterfall_MgC_ha_year))
lines(x=ListDate,y=myFunc(x=as.numeric(ListDate-min(ListDate)),
                          a = 365, b = .0002, c = 120,Base=4,p=3),col="red")

dados_an$Jour<-as.numeric(dados_an$date-min(dados_an$date,na.rm=TRUE))
NLS<-nls(data=dados_an,formula=total_litterfall_MgC_ha_year~myFunc(x=Jour, a = a, b = b, c = c,Base=Base),
         start=c(a = 365, b = .0004, c = 120,Base=4,p=3),control=list(maxiter=5000))
predNLS<-predict(NLS,newdata=data.frame(Jour=ListDate))
lines(x=ListDate,y=predNLS,col="cyan")

with(data=dados_an[dados_an$date>as.Date("2020-01-01")&dados_an$date<as.Date("2021-01-01"), ],plot(x=date,y=total_litterfall_MgC_ha_year,log="",col=sub_plot))
with(data=dados_an,plot(x=date,y=total_litterfall_MgC_ha_year,log="",col=sub_plot))

for (subPlot in unique(dados_an$sub_plot)){
  with(data=dados_an[dados_an$sub_plot==subPlot,],plot(x=date,y=total_litterfall_MgC_ha_year,main=subPlot,type="b"))
}

dados_an <- dados_an%>% group_by(plot_code, date) %>%
  summarize(total_litterfall_MgC_ha_year = sum(total_litterfall_MgC_ha_year, na.rm = TRUE))
