################################################################
#             SWISS WATCHES INDUSTRY FORECASTS                 #
################################################################

# Author: Elio Bolliger


# ==============================================================
# This file  requires a preliminary run of various_proc_TS.R
# ==============================================================

# The file produces forecasts for the following four regions:
# WORLD, MIDDLE EAST, FAR EAST, EUROPE

# It compares a final selection of models, in particular VAR and 
# ARMAX . Also, it takes into account tourism.

# ==============================================================
# Preliminaries: Load Packages, set directory, functions, variables
# ==============================================================

# clear variables:
rm(list=ls(all=TRUE))

# set the path
setwd("C:/Users/eliob/Dropbox/7_ProjektHomepage/ForecastProject/Codes")



# ----------------- INPUT NEEDED ----------------------
# QUARTERLY DATE OF THE LAST DATA ENTRY OF THE EXPORTS OF SWISS WATHES
# Format of the date: 2018.0 <- first quarter of 2018 - last month of observations is March
#                     2018.25 <- second quarter of 2018 - last month of observations is June
#                     2018.5 <- third quarter of 2018 - last month of observations is September
#                     2018.75 <- fourth quarter of 2018 - last month of observations is December
end.date.exports <- 2018.25


# LAST ENTRY OF THE DATA NEEDED FOR FORECASTS
# for example, if we are in 2019Q2 and we calculate the forecasts,
# the last data needed for the exogenous variables are 6 months after
# the 2019Q2, in more detail, 2020Q4
end.date.forecasts <- 2019.75


# ------------------- INPUT END -----------------------

# call the function file
source("various_proc_TS.R")

# call variables
source("8_1_Preliminaries.R")


if (!require("processx")) install.packages("processx")
if (!require("extrafont")) install.packages("extrafont")
if (!require("dplyr")) {install.packages("dplyr"); library('dplyr')}
if (!require("hrbrthemes")) {install.packages("hrbrthemes"); library('hrbrthemes')}
if (!require("plotly")) {install.packages("plotly"); library('plotly')}

###########################################################################################


pricerange <- read.csv("C:/Users/eliob/Dropbox/7_ProjektHomepage/ForecastProject/Codes/data_priceRange.csv")


# MONTHLY DATA
midE200 <- na.omit(as.numeric(as.character(unlist(pricerange[[2]])))) # 
midE500 <- na.omit(as.numeric(as.character(unlist(pricerange[[3]])))) # m
midE3000 <- na.omit(as.numeric(as.character(unlist(pricerange[[4]])))) # f
midE3000p <- na.omit(as.numeric(as.character(unlist(pricerange[[5]])))) # 


fe200 <- na.omit(as.numeric(as.character(unlist(pricerange[[6]])))) # w
fe500 <- na.omit(as.numeric(as.character(unlist(pricerange[[7]])))) #    o
fe3000 <- na.omit(as.numeric(as.character(unlist(pricerange[[8]])))) # far 
fe3000p <- na.omit(as.numeric(as.character(unlist(pricerange[[9]])))) # 

eu200 <- na.omit(as.numeric(as.character(unlist(pricerange[[10]])))) #    
eu500 <- na.omit(as.numeric(as.character(unlist(pricerange[[11]])))) #    of 
eu3000 <- na.omit(as.numeric(as.character(unlist(pricerange[[12]])))) # 
eu3000p <- na.omit(as.numeric(as.character(unlist(pricerange[[13]])))) # eu  of 


us200 <- na.omit(as.numeric(as.character(unlist(pricerange[[14]])))) # 
us500 <- na.omit(as.numeric(as.character(unlist(pricerange[[15]])))) # m
us3000 <- na.omit(as.numeric(as.character(unlist(pricerange[[16]])))) # 
us3000p <- na.omit(as.numeric(as.character(unlist(pricerange[[17]])))) 
us4 <- na.omit(as.numeric(as.character(unlist(pricerange[[18]])))) 
eu4 <- na.omit(as.numeric(as.character(unlist(pricerange[[19]])))) 
fe4 <- na.omit(as.numeric(as.character(unlist(pricerange[[20]])))) 
midE4 <- na.omit(as.numeric(as.character(unlist(pricerange[[21]])))) 

pet.index <- na.omit(as.numeric(as.character(unlist(pricerange[[22]])))) 

# midE
midE200 <- ts(midE200,start=c(1995,1),frequency = 12)
midE500 <- ts(midE500,start=c(1995,1),frequency = 12)
midE3000 <- ts(midE3000,start=c(1995,1),frequency = 12)
midE3000p <- ts(midE3000p,start=c(1995,1),frequency = 12)
midE4 <- ts(midE4,start=c(1995,1),frequency =12)

# eu
eu200 <- ts(eu200,start=c(1995,1),frequency = 12)
eu500 <- ts(eu500,start=c(1995,1),frequency = 12)
eu3000 <- ts(eu3000,start=c(1995,1),frequency = 12)
eu3000p <- ts(eu3000p,start=c(1995,1),frequency = 12)
eu4 <- ts(eu4,start=c(1995,1),frequency =12)

# fe
fe200 <- ts(fe200,start=c(1995,1),frequency = 12)
fe500 <- ts(fe500,start=c(1995,1),frequency = 12)
fe3000 <- ts(fe3000,start=c(1995,1),frequency = 12)
fe3000p <- ts(fe3000p,start=c(1995,1),frequency = 12)
fe4 <- ts(fe4,start=c(1995,1),frequency =12)

# us
us200 <- ts(us200,start=c(1995,1),frequency = 12)
us500 <- ts(us500,start=c(1995,1),frequency = 12)
us3000 <- ts(us3000,start=c(1995,1),frequency = 12)
us3000p <- ts(us3000p,start=c(1995,1),frequency = 12)
us4 <- ts(us4,start=c(1995,1),frequency =12)


pet.index <- ts(pet.index,start=c(1995,1),frequency = 12)





##################### DATAFRAME #####################


### TREND IN LEVEL ###
# Add the trend series (quarterly). However, we need to take the exponential to have it de-logarithmarized
df <- data.frame(ln.world.expW.q.trend)
df$date <- time(ln.world.expW.q.trend)

df <- cbind(df,ln.midE.expW.q.trend,ln.eu.expW.q.trend,ln.us.expW.q.trend,ln.fe.expW.q.trend)

df <- df %>%
  select(date, everything())

df[,2:ncol(df)] <- apply(df[,2:ncol(df)],2,exp)

colnames(df) <- c("date","world.expW.q.trend","midE.expW.q.trend","eu.expW.q.trend","us.expW.q.trend","fe.expW.q.trend")

### GROWTH RATES ###
# Now, we first create a separate dataframe for the growth rates and merge subsequently the two frames

df.g <- data.frame(ln.world.expWg.q.trend)

df.g$date <- time(ln.world.expWg.q.trend)

df.g <- cbind(df.g,ln.midE.expWg.q.trend,ln.eu.expWg.q.trend,ln.us.expWg.q.trend,ln.fe.expWg.q.trend)

df.g <- df.g %>%
  select(date, everything())




# Wait, we start with an exemplary world dataframe

df.world <- data.frame(exp(ln.world.expW.q.trend))
colnames(df.world) <- "level.q"
df.world$date <- time(ln.world.expW.q.trend)
df.world$country <- "Total"
df.world$date <- as.Date(df.world$date)
df.world$level.q.sc <- df.world$level.q/1000000


df.world <- df.world %>%
  select(date, everything()) %>%
  group_by(country) %>% 
  mutate(growth.q = 100*((level.q/lag(level.q,1))-1))
  
# add monthly series
df.world.m <- data.frame(world.expW.m)
colnames(df.world.m) <- "level.m"
df.world.m$date <- time(world.expW.m)
df.world.m$date <- as.Date(df.world.m$date)
df.world.m$country <- "Total"
df.world.m$level.m.sc <- df.world.m$level.m/1000000

# merge monthly with quarterly information
df.final <- left_join(df.world.m, df.world, by = c("date", "country"))

##################################################################################################################
# FIGURES 1


df.fig1 <- df.final %>%
  filter(date > "2008-01-01")
fig1 <- plot_ly(df.fig1,x = ~date,  y = ~level.m.sc,  name = "Monthly Exports",  type = "bar" , color = I("#ff6361"))
fig1 <- fig1 %>%   layout(title = "Total Exports of a Swiss Industry",
                          xaxis = list(title = "Date",
                                       zeroline = FALSE),
                          yaxis = list(title = "Exports in Million CHF",
                                       zeroline = FALSE))


# fig1 <- plot_ly(df.final,x = ~date,  y = ~level.m.sc,  name = "Monthly Exports",  type = "bar" , color = I("#ff6361"))
# fig1 <- fig1 %>%   layout(title = "Total Exports of a Swiss Industry",
#                           xaxis = list(title = "Date",
#                                        zeroline = FALSE),
#                           yaxis = list(title = "Exports in Million CHF",
#                                        zeroline = FALSE))


wd.homepage <- "C:/Users/eliob/Dropbox/7_ProjektHomepage/elio-rico.github.io"
save.figure.path <- file.path(wd.homepage, "forecasts_figures/totalExports_fig1.html")

htmlwidgets::saveWidget(as_widget(fig1), save.figure.path)

##################################################################################################################
# FIGURES 2


# TOTAL EXPORTS WITH TREND SERIES
df.fig2 <- df.final %>%
  filter(date > "2008-01-01") %>%
  select(date, level.q.sc, level.m.sc) %>%
  gather(key = "variable", value = "value", -date) %>%
  mutate(variable.desc = ifelse( variable == "level.q.sc", "Trend Exports, Quarterly",  "Raw Series, Monthly")) %>%
  na.omit() 


fig2 <-  plot_ly(df.fig2,x = ~date, y = ~value, color = ~variable.desc, colors =  c("#ff6361", "#003f5c"))
fig2 <- fig2 %>% add_lines()
fig2 <- fig2 %>%   layout(title = "Monthly Exports and Trend",
                        xaxis = list(title = "Date",
                                     zeroline = FALSE),
                        yaxis = list(title = "Exports in Million CHF",
                                     zeroline = FALSE))
fig2 <- fig2 %>% layout(legend = list(x = 0.7, y = 0.9))

wd.homepage <- "C:/Users/eliob/Dropbox/7_ProjektHomepage/elio-rico.github.io"
save.figure.path <- file.path(wd.homepage, "forecasts_figures/totalExports_fig2.html")

htmlwidgets::saveWidget(as_widget(fig2), save.figure.path)

##################################################################################################################
# FIGURE 3

df.expg.q <-data.frame(ch.expg.q.adj.length)
df.expg.q$date <- time(ch.expg.q.adj.length)
df.expg.q$date <- as.Date(df.expg.q$date)


df.expWg.q.trend<-data.frame(ln.world.expWg.q.trend)
df.expWg.q.trend$date <- time(ln.world.expWg.q.trend)
df.expWg.q.trend$date <- as.Date(df.expWg.q.trend$date)



df.worldrGDPg.q <-data.frame(world.rGDPg.q.adj.length)
df.worldrGDPg.q$date <- time(world.rGDPg.q.adj.length)
df.worldrGDPg.q$date <- as.Date(df.worldrGDPg.q$date)

df.final <- merge(df.final, df.expg.q,by=c("date"), all.x = TRUE)
df.final <- merge(df.final, df.worldrGDPg.q,by=c("date"), all.x = TRUE)

df.final <- merge(df.final, df.expWg.q.trend,by=c("date"), all.x = TRUE)


df.fig3 <- df.final %>%
  select(date,ch.expg.q.adj.length,ln.world.expWg.q.trend,world.rGDPg.q.adj.length)  %>%
  na.omit() %>%
  gather(key = "variable", value = "value", -date) %>%
  mutate(variable.desc = case_when(variable == "ch.expg.q.adj.length" ~ "Total Exports of Switzerland",
                                   variable == "ln.world.expWg.q.trend" ~ "Total Exports of Swiss Industry",
                         TRUE ~ "World Real GDP Growth"))



fig3 <-  plot_ly(df.fig3,x = ~date,  y = ~value, colors =  c("#003f5c", "#bc5090",'#ffa600') ) %>%
  add_lines(linetype = ~variable.desc, color = ~variable.desc) 
fig3 <- fig3 %>%   layout(title = "Correlations with Exports of Swiss Industry",
                          xaxis = list(title = "Date"),
                          yaxis = list(title = "Growth Rate (%)"))
fig3 <- fig3 %>% layout(legend = list(x = 0.8, y = 0.9))

wd.homepage <- "C:/Users/eliob/Dropbox/7_ProjektHomepage/elio-rico.github.io"
save.figure.path <- file.path(wd.homepage, "forecasts_figures/totalExports_fig3.html")

htmlwidgets::saveWidget(as_widget(fig3), save.figure.path)


##################################################################################################################


# In-sample fit different models


































##################### ##################### #####################




pdf(file = paste(figuresForecast,"midEoilGDP.pdf"))
par(mar=c(5,4,4,5)+.1)
plot(pet.index,type="l",col="black",main="Petrol Price Index (black) and GDP",ylab="")
par(new=TRUE)
plot(fe.rGDPg.q.9422, type="l",col="orange",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
#mtext("fe.rGDPg.q.9422",side=4,line=3)
#legend("topleft",col=c("red","blue"),lty=1,legend=c("pet.index","fe.rGDPg.q.9422"))
dev.off()

pdf(file = paste(figuresForecast,"midEoilexports.pdf"))
par(mar=c(5,4,4,5)+.1)
plot(pet.index,type="l",col="black",main="Indice des prix du pétrole (noir) et les exportations",ylab="")
par(new=TRUE)
plot(ln.fe.expWg.q.trend, type="l",col="orange",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
#mtext("fe.rGDPg.q.9422",side=4,line=3)
#legend("topleft",col=c("red","blue"),lty=1,legend=c("pet.index","fe.rGDPg.q.9422"))
dev.off()



# set the path
setwd("C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/Codes")
source("7_3_Forecasts_mitallenUS.R")

# developmnet of forecast variables: 
pdf(file = "C:/Users/eliob/Dropbox/CREA/Presentation/Figures/pibeumidE.pdf")
plot(eu.rGDPg.q.9422,xlab="Time",ylab="",ylim=c(-2,8),xlim=c(2011,2022),main="PIB Europe et Moyen-Orient")
lines(midE.rGDPg.q.9422,col="orange")
abline(h=0,col = "grey",lty=2)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Presentation/Figures/pibusfe.pdf")
plot(us.rGDPg.q.9422,xlab="Time",ylab="",ylim=c(-2,8),xlim=c(2011,2022),main="PIB États-Unis et Êxtreme-Orient")
lines(fe.rGDPg.q.9422,col="orange")
abline(h=0,col = "grey",lty=2)
dev.off()







# ###### proportions first:


us4.sum <- sum(us4)
us200.sum <- sum(us200)
us500.sum <- sum(us500)
us3000.sum <- sum(us3000)
us3000p.sum <- sum(us3000p)

us.prop200 <- us200.sum/us4.sum*100
us.prop500 <- us500.sum/us4.sum*100
us.prop3000 <- us3000.sum/us4.sum*100
us.prop3000p <- us3000p.sum/us4.sum*100




midE4.sum <- sum(midE4)
midE200.sum <- sum(midE200)
midE500.sum <- sum(midE500)
midE3000.sum <- sum(midE3000)
midE3000p.sum <- sum(midE3000p)

midE.prop200 <- midE200.sum/midE4.sum*100
midE.prop500 <- midE500.sum/midE4.sum*100
midE.prop3000 <- midE3000.sum/midE4.sum*100
midE.prop3000p <- midE3000p.sum/midE4.sum*100





eu4.sum <- sum(eu4)
eu200.sum <- sum(eu200)
eu500.sum <- sum(eu500)
eu3000.sum <- sum(eu3000)
eu3000p.sum <- sum(eu3000p)

eu.prop200 <- eu200.sum/eu4.sum*100
eu.prop500 <- eu500.sum/eu4.sum*100
eu.prop3000 <- eu3000.sum/eu4.sum*100
eu.prop3000p <- eu3000p.sum/eu4.sum*100




fe4.sum <- sum(fe4)
fe200.sum <- sum(fe200)
fe500.sum <- sum(fe500)
fe3000.sum <- sum(fe3000)
fe3000p.sum <- sum(fe3000p)

fe.prop200 <- fe200.sum/fe4.sum*100
fe.prop500 <- fe500.sum/fe4.sum*100
fe.prop3000 <- fe3000.sum/fe4.sum*100
fe.prop3000p <- fe3000p.sum/fe4.sum*100




# midE
midE200 <- log(midE200)
midE500 <- log(midE500)
midE3000 <- log(midE3000)
midE3000p <- log(midE3000p)

# us
us200 <- log(us200)
us500 <- log(us500)
us3000 <- log(us3000)
us3000p <- log(us3000p)


# fe
fe200 <- log(fe200)
fe500 <- log(fe500)
fe3000 <- log(fe3000)
fe3000p <- log(fe3000p)

# eu
eu200 <- log(eu200)
eu500 <- log(eu500)
eu3000 <- log(eu3000)
eu3000p <- log(eu3000p)



midE200.deseas <- stl(midE200,s.window="periodic")
midE500.deseas <- stl(midE500,s.window="periodic")
midE3000.deseas <- stl(midE3000,s.window="periodic")
midE3000p.deseas <- stl(midE3000p,s.window="periodic")


eu200.deseas <- stl(eu200,s.window="periodic")
eu500.deseas <- stl(eu500,s.window="periodic")
eu3000.deseas <- stl(eu3000,s.window="periodic")
eu3000p.deseas <- stl(eu3000p,s.window="periodic")


fe200.deseas <- stl(fe200,s.window="periodic")
fe500.deseas <- stl(fe500,s.window="periodic")
fe3000.deseas <- stl(fe3000,s.window="periodic")
fe3000p.deseas <- stl(fe3000p,s.window="periodic")


us200.deseas <- stl(us200,s.window="periodic")
us500.deseas <- stl(us500,s.window="periodic")
us3000.deseas <- stl(us3000,s.window="periodic")
us3000p.deseas <- stl(us3000p,s.window="periodic")


midE200.trend <- midE200.deseas$time.series[,2]
midE500.trend <- midE500.deseas$time.series[,2]
midE3000.trend <- midE3000.deseas$time.series[,2]
midE3000p.trend <- midE3000p.deseas$time.series[,2]

us200.trend <- us200.deseas$time.series[,2]
us500.trend <- us500.deseas$time.series[,2]
us3000.trend <- us3000.deseas$time.series[,2]
us3000p.trend <- us3000p.deseas$time.series[,2]

eu200.trend <- eu200.deseas$time.series[,2]
eu500.trend <- eu500.deseas$time.series[,2]
eu3000.trend <- eu3000.deseas$time.series[,2]
eu3000p.trend <- eu3000p.deseas$time.series[,2]


fe200.trend <- fe200.deseas$time.series[,2]
fe500.trend <- fe500.deseas$time.series[,2]
fe3000.trend <- fe3000.deseas$time.series[,2]
fe3000p.trend <- fe3000p.deseas$time.series[,2]








# normalize the series
midE200.trend.n <- midE200.trend-mean(midE200.trend)
midE200.trend.n <- midE200.trend.n/sd(midE200.trend)
midE500.trend.n <- midE500.trend-mean(midE500.trend)
midE500.trend.n <- midE500.trend.n/sd(midE500.trend)
midE3000.trend.n <- midE3000.trend-mean(midE3000.trend)
midE3000.trend.n <- midE3000.trend.n/sd(midE3000.trend)
midE3000p.trend.n <- midE3000p.trend-mean(midE3000p.trend)
midE3000p.trend.n <- midE3000p.trend.n/sd(midE3000p.trend)


fe200.trend.n <- fe200.trend-mean(fe200.trend)
fe200.trend.n <- fe200.trend.n/sd(fe200.trend)
fe500.trend.n <- fe500.trend-mean(fe500.trend)
fe500.trend.n <- fe500.trend.n/sd(fe500.trend)
fe3000.trend.n <- fe3000.trend-mean(fe3000.trend)
fe3000.trend.n <- fe3000.trend.n/sd(fe3000.trend)
fe3000p.trend.n <- fe3000p.trend-mean(fe3000p.trend)
fe3000p.trend.n <- fe3000p.trend.n/sd(fe3000p.trend)


eu200.trend.n <- eu200.trend-mean(eu200.trend)
eu200.trend.n <- eu200.trend.n/sd(eu200.trend)
eu500.trend.n <- eu500.trend-mean(eu500.trend)
eu500.trend.n <- eu500.trend.n/sd(eu500.trend)
eu3000.trend.n <- eu3000.trend-mean(eu3000.trend)
eu3000.trend.n <- eu3000.trend.n/sd(eu3000.trend)
eu3000p.trend.n <- eu3000p.trend-mean(eu3000p.trend)
eu3000p.trend.n <- eu3000p.trend.n/sd(eu3000p.trend)



us200.trend.n <- us200.trend-mean(us200.trend)
us200.trend.n <- us200.trend.n/sd(us200.trend)
us500.trend.n <- us500.trend-mean(us500.trend)
us500.trend.n <- us500.trend.n/sd(us500.trend)
us3000.trend.n <- us3000.trend-mean(us3000.trend)
us3000.trend.n <- us3000.trend.n/sd(us3000.trend)
us3000p.trend.n <- us3000p.trend-mean(us3000p.trend)
us3000p.trend.n <- us3000p.trend.n/sd(us3000p.trend)



pdf(file = "C:/Users/eliob/Dropbox/CREA/Presentation/Figures/midE_normalized.pdf")
plot(midE200.trend.n, col="grey",lty=2,main="Moyen-Orient: séries temporelles normalisées",ylab="")
lines(midE500.trend.n,col="blue",lty=2)
lines(midE3000.trend.n, col="orange")
lines(midE3000p.trend.n, col="black")
dev.off()



pdf(file = "C:/Users/eliob/Dropbox/CREA/Presentation/Figures/fe_normalized.pdf")
plot(fe200.trend.n, col="grey",lty=2,main="Êxtreme-Orient: séries temporelles normalisées",ylab="")
lines(fe500.trend.n,col="blue",lty=2)
lines(fe3000.trend.n, col="orange")
lines(fe3000p.trend.n, col="black")
dev.off()



pdf(file = "C:/Users/eliob/Dropbox/CREA/Presentation/Figures/eu_normalized.pdf")
plot(eu200.trend.n, col="grey",lty=2,main="Europe: séries temporelles normalisées",ylab="")
lines(eu500.trend.n,col="blue",lty=2)
lines(eu3000.trend.n, col="orange")
lines(eu3000p.trend.n, col="black")
dev.off()


pdf(file = "C:/Users/eliob/Dropbox/CREA/Presentation/Figures/us_normalized.pdf")
plot(us200.trend.n, col="grey",lty=2,main="États-Unis: séries temporelles normalisées",ylab="")
lines(us500.trend.n,col="blue",lty=2)
lines(us3000.trend.n, col="orange")
lines(us3000p.trend.n, col="black")
dev.off()





# Proportions of the price ranges:




# ==============================================================
# Forecasts for 2018Q1
# ==============================================================








# ARMAX Forecasts:

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# World
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




# Prepare xreg matrix:

# xreg to estimate the moel
xreg.world.q <- cbind(world.rGDPg.q.l1,ch.expg.q.l1,ch.rfx.q.l1)

# xreg for forecast:
xreg.F.world.q <- cbind(world.rGDPg.q.F.l1,ch.expg.q.F.l1,ch.rfx.q.F.l1)

# estimate the model:
world.armax.q <- auto.arima(ln.world.expWg.q.trend, xreg = xreg.world.q)
FC.world.armax.q <- forecast(world.armax.q, h=6,xreg=xreg.F.world.q)

# 
# pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/world_armax_forecast.pdf")
# plot(FC.world.armax.q,ylab = "Growth rate in %, Q-Q")
# lines(fitted(world.armax.q), col = "orange")
# abline(h = 0, col = "red")
# dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/world_armax_forecast_withoutci.pdf")
p <- forecastARMAX_CI_2(ln.world.expWg.q.trend,6,1994,1,0,2007,2018.25,xreg.world.q,xreg.F.world.q)
dev.off()


# insample:
pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/world_armax_insample1.pdf")
insample <- insampleARMA_X(ln.world.expWg.q.trend,6,1994,1,50,xreg.world.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/world_armax_insample2.pdf")
insample <- insampleARMA_X(ln.world.expWg.q.trend,6,1994,1,60,xreg.world.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/world_armax_insample3.pdf")
insample <- insampleARMA_X(ln.world.expWg.q.trend,6,1994,1,70,xreg.world.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/world_armax_insample4.pdf")
insample <- insampleARMA_X(ln.world.expWg.q.trend,6,1994,1,80,xreg.world.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/world_armax_insample5.pdf")
insample <- insampleARMA_X(ln.world.expWg.q.trend,6,1994,1,90,xreg.world.q)
dev.off()


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# midE
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Prepare xreg matrix:

# xreg to estimate the moel
xreg.midE.q <- cbind(midE.rGDPg.q.l1,ch.impg.q.l1,ch.expg.q.l1,ch.rfx.q.l1)

# xreg for forecast:
xreg.F.midE.q <- cbind(midE.rGDPg.q.F.l1,ch.impg.q.F.l1,ch.expg.q.F.l1,ch.rfx.q.F.l1)

# estimate the model:
midE.armax.q <- auto.arima(ln.midE.expWg.q.trend, xreg = xreg.midE.q)
FC.midE.armax.q <- forecast(midE.armax.q, h=6,xreg=xreg.F.midE.q)


# pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/midE_armax_forecast.pdf")
# plot(FC.midE.armax.q,ylab = "Growth rate in %, Q-Q")
# lines(fitted(midE.armax.q), col = "orange")
# abline(h = 0, col = "red")
# dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/midE_armax_forecast_withoutci.pdf")
p <- forecastARMAX_CI_2(ln.midE.expWg.q.trend,6,1994,1,0,2007,2018.25,xreg.midE.q,xreg.F.midE.q)
dev.off()

# insample:
pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/midE_armax_insample1.pdf")
insample <- insampleARMA_X(ln.midE.expWg.q.trend,6,1994,1,50,xreg.midE.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/midE_armax_insample2.pdf")
insample <- insampleARMA_X(ln.midE.expWg.q.trend,6,1994,1,60,xreg.midE.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/midE_armax_insample3.pdf")
insample <- insampleARMA_X(ln.midE.expWg.q.trend,6,1994,1,70,xreg.midE.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/midE_armax_insample4.pdf")
insample <- insampleARMA_X(ln.midE.expWg.q.trend,6,1994,1,80,xreg.midE.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/midE_armax_insample5.pdf")
insample <- insampleARMA_X(ln.midE.expWg.q.trend,6,1994,1,90,xreg.midE.q)
dev.off()



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# fe
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Prepare xreg matrix:

# xreg to estimate the moel
xreg.fe.q <- cbind(fe.rGDPg.q.l1,ch.expg.q.l1,ch.rfx.q.l1)

# xreg for forecast:
xreg.F.fe.q <- cbind(fe.rGDPg.q.F.l1,ch.expg.q.F.l1,ch.rfx.q.F.l1)

# estimate the model:
fe.armax.q <- auto.arima(ln.fe.expWg.q.trend, xreg = xreg.fe.q)
FC.fe.armax.q <- forecast(fe.armax.q, h=6,xreg=xreg.F.fe.q)


# pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/fe_armax_forecast.pdf")
# plot(FC.fe.armax.q,ylab = "Growth rate in %, Q-Q")
# lines(fitted(fe.armax.q), col = "orange")
# abline(h = 0, col = "red")
# dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/fe_armax_forecast_withoutci.pdf")
p <- forecastARMAX_CI_2(ln.fe.expWg.q.trend,6,1994,1,0,2007,2018.25,xreg.fe.q,xreg.F.fe.q)
dev.off()

# Insample fit:

# insample:
pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/fe_armax_insample1.pdf")
insample <- insampleARMA_X(ln.fe.expWg.q.trend,6,1994,1,50,xreg.fe.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/fe_armax_insample2.pdf")
insample <- insampleARMA_X(ln.fe.expWg.q.trend,6,1994,1,60,xreg.fe.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/fe_armax_insample3.pdf")
insample <- insampleARMA_X(ln.fe.expWg.q.trend,6,1994,1,70,xreg.fe.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/fe_armax_insampleyolo.pdf")
insample <- insampleARMA_X(ln.fe.expWg.q.trend,6,1994,1,80,xreg.fe.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/fe_armax_insample5.pdf")
insample <- insampleARMA_X(ln.fe.expWg.q.trend,6,1994,1,90,xreg.fe.q)
dev.off()




# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# eu
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Prepare xreg matrix:

# xreg to estimate the moel
xreg.eu.q <- cbind(eu.rGDPg.q.l1,ch.expg.q.l1,ch.rfx.q.l1)

# xreg for forecast:
xreg.F.eu.q <- cbind(eu.rGDPg.q.F.l1,ch.expg.q.F.l1,ch.rfx.q.F.l1)

# estimate the model:
eu.armax.q <- auto.arima(ln.eu.expWg.q.trend, xreg = xreg.eu.q)
FC.eu.armax.q <- forecast(eu.armax.q, h=6,xreg=xreg.F.eu.q)


# pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/eu_armax_forecast.pdf")
# plot(FC.eu.armax.q,ylab = "Growth rate in %, Q-Q")
# lines(fitted(eu.armax.q), col = "orange")
# abline(h = 0, col = "red")
# dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/eu_armax_forecast_withoutci.pdf")
p <- forecastARMAX_CI_2(ln.eu.expWg.q.trend,6,1994,1,0,2007,2018.25,xreg.eu.q,xreg.F.eu.q)
dev.off()


# insample:
pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/eu_armax_insample1.pdf")
insample <- insampleARMA_X(ln.eu.expWg.q.trend,6,1994,1,50,xreg.eu.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/eu_armax_insample2.pdf")
insample <- insampleARMA_X(ln.eu.expWg.q.trend,6,1994,1,60,xreg.eu.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/eu_armax_insample3.pdf")
insample <- insampleARMA_X(ln.eu.expWg.q.trend,6,1994,1,70,xreg.eu.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/eu_armax_insample4.pdf")
insample <- insampleARMA_X(ln.eu.expWg.q.trend,6,1994,1,80,xreg.eu.q)
dev.off()

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/eu_armax_insample5.pdf")
insample <- insampleARMA_X(ln.eu.expWg.q.trend,6,1994,1,90,xreg.eu.q)
dev.off()




# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Calculate the level of the exports:
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# First, we calculate the level of the existing series:
# eu
eu.expW.q.trend.level <- exp(ln.eu.expW.q.trend)

# midE
midE.expW.q.trend.level <- exp(ln.midE.expW.q.trend)

# fe
fe.expW.q.trend.level <- exp(ln.fe.expW.q.trend)

# world
world.expW.q.trend.level <- exp(ln.world.expW.q.trend)


# Now we have to calculate the new levels, given the forecasts of the growth rate

# world
forecast.level.world <- 0
for (i in 1:length(FC.world.armax.q$mean)) {
  forecast.level.world[1] <- world.expW.q.trend.level[length(world.expW.q.trend.level)]
  forecast.level.world[i+1] <- forecast.level.world[i]*(FC.world.armax.q$mean[i]/100 + 1)
 
}
forecast.level.world <- ts(c(world.expW.q.trend.level,forecast.level.world[2:length(forecast.level.world)]),start=c(1994,1),frequency = 4)

# fe
forecast.level.fe <- 0
for (i in 1:length(FC.fe.armax.q$mean)) {
  forecast.level.fe[1] <- fe.expW.q.trend.level[length(fe.expW.q.trend.level)]
  forecast.level.fe[i+1] <- forecast.level.fe[i]*(FC.fe.armax.q$mean[i]/100 + 1)
  
}
forecast.level.fe <- ts(c(fe.expW.q.trend.level,forecast.level.fe[2:length(forecast.level.fe)]),start=c(1994,1),frequency = 4)



# midE
forecast.level.midE <- 0
for (i in 1:length(FC.midE.armax.q$mean)) {
  forecast.level.midE[1] <- midE.expW.q.trend.level[length(midE.expW.q.trend.level)]
  forecast.level.midE[i+1] <- forecast.level.midE[i]*(FC.midE.armax.q$mean[i]/100 + 1)
  
}
forecast.level.midE <- ts(c(midE.expW.q.trend.level,forecast.level.midE[2:length(forecast.level.midE)]),start=c(1994,1),frequency = 4)



# eu
forecast.level.eu <- 0
for (i in 1:length(FC.eu.armax.q$mean)) {
  forecast.level.eu[1] <- eu.expW.q.trend.level[length(eu.expW.q.trend.level)]
  forecast.level.eu[i+1] <- forecast.level.eu[i]*(FC.eu.armax.q$mean[i]/100 + 1)
  
}
forecast.level.eu <- ts(c(eu.expW.q.trend.level,forecast.level.eu[2:length(forecast.level.eu)]),start=c(1994,1),frequency = 4)


  
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# PLOTS
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


t <- time(forecast.level.world)
tmax <- t[(length(forecast.level.world))]

# world

# transform to Mrd. CHF
forecast.level.world.mrd <- forecast.level.world/1000000000
world.expW.q.trend.level.mrd <- world.expW.q.trend.level/1000000000
world.expW.m.mrd <- (exp(ln.world.expW.m))/1000000000


minimum <- min(world.expW.m.mrd[200:length(world.expW.m.mrd)])
maximum <- max(world.expW.m.mrd[200:length(world.expW.m.mrd)])

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/world_armax_forecast_level.pdf")
plot(forecast.level.world.mrd,col="blue",ylab="Niveau des exportations, Mrd. CHF", xlim=c(2011,tmax),xlab="",lwd=2,ylim = range(minimum,maximum))
lines(world.expW.q.trend.level.mrd,col="black",lwd=2)
lines(world.expW.m.mrd,col="grey",lty = 2)
dev.off()

# fe
forecast.level.fe.mrd <- forecast.level.fe/1000000000
fe.expW.q.trend.level.mrd <- fe.expW.q.trend.level/1000000000
fe.expW.m.mrd <- (exp(ln.fe.expW.m))/1000000000


minimum <- min(fe.expW.m.mrd[200:length(fe.expW.m.mrd)])
maximum <- max(fe.expW.m.mrd[200:length(fe.expW.m.mrd)])

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/fe_armax_forecast_level.pdf")
plot(forecast.level.fe.mrd,col="blue",ylab="Niveau des exportations, Mrd. CHF", xlim=c(2011,tmax),xlab="",lwd=2,ylim = range(minimum,maximum))
lines(fe.expW.q.trend.level.mrd,col="black",lwd=2)
lines(fe.expW.m.mrd,col="grey",lty = 2)
dev.off()


# midE
forecast.level.midE.mrd <- forecast.level.midE/1000000000
midE.expW.q.trend.level.mrd <- midE.expW.q.trend.level/1000000000
midE.expW.m.mrd <- (exp(ln.midE.expW.m))/1000000000


minimum <- min(midE.expW.m.mrd[200:length(midE.expW.m.mrd)])
maximum <- max(midE.expW.m.mrd[200:length(midE.expW.m.mrd)])

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/midE_armax_forecast_level.pdf")
plot(forecast.level.midE.mrd,col="blue",ylab="Niveau des exportations, Mrd. CHF", xlim=c(2011,tmax),xlab="",lwd=2,ylim = range(minimum,maximum))
lines(midE.expW.q.trend.level.mrd,col="black",lwd=2)
lines(midE.expW.m.mrd,col="grey",lty = 2)
dev.off()

# eu
forecast.level.eu.mrd <- forecast.level.eu/1000000000
eu.expW.q.trend.level.mrd <- eu.expW.q.trend.level/1000000000
eu.expW.m.mrd <- (exp(ln.eu.expW.m))/1000000000


minimum <- min(eu.expW.m.mrd[200:length(eu.expW.m.mrd)])
maximum <- max(eu.expW.m.mrd[200:length(eu.expW.m.mrd)])

pdf(file = "C:/Users/eliob/Dropbox/CREA/Codes/7_Seventh_Forecast/FiguresForecast/eu_armax_forecast_level.pdf")
plot(forecast.level.eu.mrd,col="blue",ylab="Niveau des exportations, Mrd. CHF", xlim=c(2011,tmax),xlab="",lwd=2,ylim = range(minimum,maximum))
lines(eu.expW.q.trend.level.mrd,col="black",lwd=2)
lines(eu.expW.m.mrd,col="grey",lty = 2)
dev.off()







######################################################################################################################################################  
######################################################################################################################################################  
######################################################################################################################################################  
######################################################################################################################################################  