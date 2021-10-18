################################################################
#             SWISS WATCHES INDUSTRY FORECASTS                 #
################################################################

# Author: Elio Bolliger


# ==============================================================
# This file  requires a preliminary run of various_proc_TS.R
# ==============================================================


# ==============================================================
# Preliminaries: Load Packages, set directory, functions
# ==============================================================



# LOAD THE PACKAGES
if (!require("tseries")) {install.packages("tseries"); library('tseries')}
if (!require("readxl")) {install.packages("readxl"); library('readxl')}
if (!require("zoom")) {install.packages("zoom"); library('zoom')}
if (!require("lmtest")) {install.packages("lmtest"); library('lmtest')}
if (!require("vars")) {install.packages("vars"); library('vars')}
if (!require("forecast")) {install.packages("forecast"); library('forecast')}
if (!require("tis")) {install.packages("tis"); library('tis')}
# Packages for seasonal adjustment:
if (!require("seasonal")) {install.packages("seasonal"); library('seasonal')}
if (!require("reshape2")) {install.packages("reshape2"); library('reshape2')}
if (!require("grid")) {install.packages("grid"); library('grid')}
if (!require("scales")) {install.packages("scales"); library('scales')}
if (!require("lubridate")) {install.packages("lubridate"); library('lubridate')}
if (!require("tidyverse")) {install.packages("tidyverse"); library('tidyverse')}


# ==============================================================
# 1) Load the data for all four regions: 
# ==============================================================


# READ IN THE DATA 

# Make sure the data is updated!
data_yearly <- read.csv("data_yearly.csv")
data_monthly <- read.csv("data_monthly.csv")



# Convert the data needed into numeric values and timer series:

# YEARLY DATA
# convert into numeric values
world.rGDPg.y <- as.numeric(as.character(unlist(data_yearly[[2]]))) # world real GDP growth
midE.rGDPg.y <- as.numeric(as.character(unlist(data_yearly[[3]]))) # middle east real GDP growth
fe.rGDPg.y <- as.numeric(as.character(unlist(data_yearly[[4]]))) # far east real GDP growth
eu.rGDPg.y <- as.numeric(as.character(unlist(data_yearly[[5]]))) # europe real GDP growth
ch.impg.y <- as.numeric(as.character(unlist(data_yearly[[6]]))) # ch imports of all goods - growth rate
ch.expg.y <- as.numeric(as.character(unlist(data_yearly[[7]]))) # ch exports of all goods - growth rate
us.rGDPg.y <- as.numeric(as.character(unlist(data_yearly[[8]]))) # US real GDP growth
us.impg.y <- as.numeric(as.character(unlist(data_yearly[[9]]))) # us imports of all goods - growth rate


# convert into time series - all time series start 1980 and go until 2022 (forecasts IMF)
world.rGDPg.y <- ts(world.rGDPg.y, start = 1980.00, frequency = 1)
midE.rGDPg.y <- ts(midE.rGDPg.y, start = 1980.00, frequency = 1)
fe.rGDPg.y <- ts(fe.rGDPg.y, start = 1980.00, frequency = 1)
eu.rGDPg.y <- ts(eu.rGDPg.y, start = 1980.00, frequency = 1)
ch.impg.y <- ts(ch.impg.y, start = 1980.00, frequency = 1)
ch.expg.y <- ts(ch.expg.y, start = 1980.00, frequency = 1)
us.rGDPg.y <- ts(us.rGDPg.y, start = 1980.00, frequency = 1)
us.impg.y <- ts(us.impg.y, start = 1980.00, frequency = 1)


# MONTHLY DATA
world.expW.m <- na.omit(as.numeric(as.character(unlist(data_monthly[[2]])))) # world exports of watches
midE.expW.m <- na.omit(as.numeric(as.character(unlist(data_monthly[[3]])))) # middle east exports of watches
fe.expW.m <- na.omit(as.numeric(as.character(unlist(data_monthly[[4]])))) # far east exports of watches
eu.expW.m <- na.omit(as.numeric(as.character(unlist(data_monthly[[5]])))) # eu exports of watches
us.expW.m <- na.omit(as.numeric(as.character(unlist(data_monthly[[6]])))) # us exports of watches
ch.rfx.m <- na.omit(as.numeric(as.character(unlist(data_monthly[[7]])))) # ch real effective exchange rate

# convert into time series - exports of watches start at january 1993, real effective exchange rate starts at january 1994
world.expW.m <- ts(world.expW.m, start = 1993.00, frequency = 12)
midE.expW.m <- ts(midE.expW.m, start = 1993.00, frequency = 12)
fe.expW.m <- ts(fe.expW.m, start = 1993.00, frequency = 12)
eu.expW.m <- ts(eu.expW.m, start = 1993.00, frequency = 12)
ch.rfx.m <- ts(ch.rfx.m, start = 1994.00, frequency = 12)
us.expW.m <- ts(us.expW.m, start = 1993.00, frequency = 12)

# ==============================================================
# 2) Frequency of the data
# ============================================================== 

# ADJUST THE FREQUENCY OF THE MONTHLY DATA

# Convert monthly data to quarterly data
world.expW.q <- convert(world.expW.m, tif = 'quarterly', observed = 'averaged')
midE.expW.q <- convert(midE.expW.m, tif = 'quarterly', observed = 'averaged')
fe.expW.q <- convert(fe.expW.m, tif = 'quarterly', observed = 'averaged')
eu.expW.q <- convert(eu.expW.m, tif = 'quarterly', observed = 'averaged')
ch.rfx.q <- convert(ch.rfx.m, tif = 'quarterly', observed = 'averaged')
us.expW.q <- convert(us.expW.m, tif = 'quarterly', observed = 'averaged')




# ==============================================================
# 3) Deseasonalize the data
# ==============================================================

# DESEASONALIZE THE MONTHLY EXPORT DATA

# To forecast the exports os Swiss Watches, we use both long term trend data and data with long term trend AND cylical component.
# This is why we first deaseonalize the monthly data. For this, we use two procedures:

# 1) Deseasonlize data 

# take log of exports:

ln.world.expW.q <- log(world.expW.q)
ln.midE.expW.q <- log(midE.expW.q)
ln.fe.expW.q <- log(fe.expW.q)
ln.eu.expW.q <- log(eu.expW.q)
ln.us.expW.q <- log(us.expW.q)

# Tests have shown that simply deseaonslizing the data is not enough for accurate forecasts. For this reason, we decided to extract the long term trend of the
# data. This trend is obtained with the LOESS method. This method estimates local regressions and smoothes the obtained curve. The advantage is no observation
# loss at the end of the data sample.

# Deseaonlize the data
ln.world.expW.q.deseas2 <- stl(ln.world.expW.q, s.window=11)
ln.midE.expW.q.deseas2 <- stl(ln.midE.expW.q, s.window=10)
ln.fe.expW.q.deseas2 <- stl(ln.fe.expW.q, s.window=10)
ln.eu.expW.q.deseas2 <- stl(ln.eu.expW.q, s.window=11)
ln.us.expW.q.deseas2 <- stl(ln.us.expW.q, s.window="periodic")


# Please note that the long-term trend data is located in the second column of the dataset (-> [,2])
ln.world.expW.q.trend <- ln.world.expW.q.deseas2$time.series[,2]
ln.midE.expW.q.trend <- ln.midE.expW.q.deseas2$time.series[,2]
ln.fe.expW.q.trend <- ln.fe.expW.q.deseas2$time.series[,2]
ln.eu.expW.q.trend <- ln.eu.expW.q.deseas2$time.series[,2]
ln.us.expW.q.trend <- ln.us.expW.q.deseas2$time.series[,2]



# ==============================================================
# 4) Frequency of the yearly data
# ==============================================================


# QUARTERLY DATA FOR FORECASTING

# Define the length of the vector of the forecasting values:
length.variables.y <- length(as.character(unlist(data_yearly[[1]]))) # us imports of all goods - growth rate

start.data <- 1980
end.data <- start.data+length.variables.y-1

#Yearly data goes from 1980 to 2022
x <- c(start.data:end.data)

# World GDP
func = splinefun(x=x, y=world.rGDPg.y, method="fmm",  ties = mean)
world.rGDPg.q <- func(seq(start.data, end.data, 0.25))

# Middle East GDP
func = splinefun(x=x, y=midE.rGDPg.y, method="fmm",  ties = mean)
midE.rGDPg.q <- func(seq(start.data, end.data, 0.25))

# Far East GDP
func = splinefun(x=x, y=fe.rGDPg.y, method="fmm",  ties = mean)
fe.rGDPg.q <- func(seq(start.data, end.data, 0.25))

# Europe GDP
func = splinefun(x=x, y=eu.rGDPg.y, method="fmm",  ties = mean)
eu.rGDPg.q <- func(seq(start.data, end.data, 0.25))

# Import growth rate CH
func = splinefun(x=x, y=ch.impg.y, method="fmm",  ties = mean)
ch.impg.q <- func(seq(start.data, end.data, 0.25))

# Export growth rate CH
func = splinefun(x=x, y=ch.expg.y, method="fmm",  ties = mean)
ch.expg.q <- func(seq(start.data, end.data, 0.25))    

# GDP growth US
func = splinefun(x=x, y=us.rGDPg.y, method="fmm",  ties = mean)
us.rGDPg.q <- func(seq(start.data, end.data, 0.25)) 

# Import growth rate US
func = splinefun(x=x, y=us.impg.y, method="fmm",  ties = mean)
us.impg.q <- func(seq(start.data, end.data, 0.25)) 


# ==============================================================
# 5) Stationarity of the time series
# ==============================================================

# ENSURE STATIONARITY OF THE TIME SERIES

# For our forecasting models we need stationary time series. For this reason, we analyze the growth rates of the exports.

ln.world.expW.q.trend.dif <- 0
for (i in 2:length(ln.world.expW.q.trend)){
  ln.world.expW.q.trend.dif[i] = (exp(ln.world.expW.q.trend[i])-exp(ln.world.expW.q.trend[i-1]))/exp(ln.world.expW.q.trend[i-1])
}
ln.world.expW.q.trend.dif <- ln.world.expW.q.trend.dif[2:length(ln.world.expW.q.trend.dif)]*100
ln.world.expW.q.trend.dif <- ts(ln.world.expW.q.trend.dif,start=1993.25,frequency = 4)

ln.midE.expW.q.trend.dif <- 0
for (i in 2:length(ln.midE.expW.q.trend)){
  ln.midE.expW.q.trend.dif[i] = (exp(ln.midE.expW.q.trend[i])-exp(ln.midE.expW.q.trend[i-1]))/exp(ln.midE.expW.q.trend[i-1])
}
ln.midE.expW.q.trend.dif <- ln.midE.expW.q.trend.dif[2:length(ln.midE.expW.q.trend.dif)]*100
ln.midE.expW.q.trend.dif <- ts(ln.midE.expW.q.trend.dif,start=1993.25,frequency = 4)

ln.fe.expW.q.trend.dif <- 0
for (i in 2:length(ln.fe.expW.q.trend)){
  ln.fe.expW.q.trend.dif[i] = (exp(ln.fe.expW.q.trend[i])-exp(ln.fe.expW.q.trend[i-1]))/exp(ln.fe.expW.q.trend[i-1])
}
ln.fe.expW.q.trend.dif <- ln.fe.expW.q.trend.dif[2:length(ln.fe.expW.q.trend.dif)]*100
ln.fe.expW.q.trend.dif <- ts(ln.fe.expW.q.trend.dif,start=1993.25,frequency = 4)

ln.eu.expW.q.trend.dif <- 0
for (i in 2:length(ln.eu.expW.q.trend)){
  ln.eu.expW.q.trend.dif[i] = (exp(ln.eu.expW.q.trend[i])-exp(ln.eu.expW.q.trend[i-1]))/exp(ln.eu.expW.q.trend[i-1])
}
ln.eu.expW.q.trend.dif <- ln.eu.expW.q.trend.dif[2:length(ln.eu.expW.q.trend.dif)]*100
ln.eu.expW.q.trend.dif <- ts(ln.eu.expW.q.trend.dif,start=1993.25,frequency = 4)

ln.us.expW.q.trend.dif <- 0
for (i in 2:length(ln.us.expW.q.trend)){
  ln.us.expW.q.trend.dif[i] = (exp(ln.us.expW.q.trend[i])-exp(ln.us.expW.q.trend[i-1]))/exp(ln.us.expW.q.trend[i-1])
}
ln.us.expW.q.trend.dif <- ln.us.expW.q.trend.dif[2:length(ln.us.expW.q.trend.dif)]*100
ln.us.expW.q.trend.dif <- ts(ln.us.expW.q.trend.dif,start=1993.25,frequency = 4)



# ==============================================================
# 5) Adjust length, define as time series
# PLEASE NOTE THAT IN THIS SECTION, WE CUT THE TIME SERIES
# SUCH THAT THEY ALL START IN 1994 UNTIL Q22018
# ==============================================================

# In this section of the code, we define the series as time series
# (if not already done) and group the variables such that they 
# can be used for the different models.

# PLEASE NOTE THAT DUE TO DATA INAVAILABILITY OF THE REAL EXCHANGE RATE OF THE SWISS FRANC, WE USE DATA STARTING
# AT 1994 EVEN THOUGH DATA FOR EXPORTS ARE AVAILABLE SINCE 1993.


# LEVEL DATA - DETRENEDED, QUARTERLY -- ADJUSTING THE LENGTH
ln.world.expW.q.trend.level <- ts(window(ln.world.expW.q.trend,start=1994.00),start=1994.00,frequency = 4)
ln.midE.expW.q.trend.level <- ts(window(ln.midE.expW.q.trend,start=1994.00),start=1994.00,frequency = 4)
ln.fe.expW.q.trend.level <- ts(window(ln.fe.expW.q.trend,start=1994.00),start=1994.00,frequency = 4)      
ln.eu.expW.q.trend.level <- ts(window(ln.eu.expW.q.trend,start=1994.00),start=1994.00,frequency = 4)      
ln.us.expW.q.trend.level <- ts(window(ln.us.expW.q.trend,start=1994.00),start=1994.00,frequency = 4)


# GROWTH RATES - DETRENDED, QUARTERLY -- ADJUSTING THE LENGTH

# we name the new series expWg for growth
ln.world.expWg.q.trend <- ts(window(ln.world.expW.q.trend.dif,start=1994.00),start=1994.00,frequency = 4)
ln.midE.expWg.q.trend <- ts(window(ln.midE.expW.q.trend.dif,start=1994.00),start=1994.00,frequency = 4)
ln.fe.expWg.q.trend <- ts(window(ln.fe.expW.q.trend.dif,start=1994.00),start=1994.00,frequency = 4)
ln.eu.expWg.q.trend <- ts(window(ln.eu.expW.q.trend.dif,start=1994.00),start=1994.00,frequency = 4)
ln.us.expWg.q.trend <- ts(window(ln.us.expW.q.trend.dif,start=1994.00),start=1994.00,frequency = 4)


# all exogenous variables, 1994 to the point indicated in end.date.exports
world.rGDPg.q <- ts(world.rGDPg.q,start = 1980.00, frequency = 4)
world.rGDPg.q.adj.length <- ts(window(world.rGDPg.q, start = 1994.00, end = end.date.exports),start=1994.00,frequency = 4)

midE.rGDPg.q <- ts(midE.rGDPg.q,start = 1980.00, frequency = 4)
midE.rGDPg.q.adj.length <- ts(window(midE.rGDPg.q, start = 1994.00, end = end.date.exports),start=1994.00,frequency = 4)

fe.rGDPg.q <- ts(fe.rGDPg.q,start = 1980.00, frequency = 4)
fe.rGDPg.q.adj.length <- ts(window(fe.rGDPg.q, start = 1994.00, end = end.date.exports),start=1994.00,frequency = 4)

eu.rGDPg.q <- ts(eu.rGDPg.q,start = 1980.00, frequency = 4)
eu.rGDPg.q.adj.length <- ts(window(eu.rGDPg.q, start = 1994.00, end = end.date.exports),start=1994.00,frequency = 4)

ch.impg.q <- ts(ch.impg.q,start = 1980.00, frequency = 4)
ch.impg.q.adj.length <- ts(window(ch.impg.q, start = 1994.00, end = end.date.exports),start=1994.00,frequency = 4)

ch.expg.q <- ts(ch.expg.q,start = 1980.00, frequency = 4)
ch.expg.q.adj.length <- ts(window(ch.expg.q, start = 1994.00, end = end.date.exports),start=1994.00,frequency = 4)

us.rGDPg.q <- ts(us.rGDPg.q,start = 1980.00, frequency = 4)
us.rGDPg.q.adj.length <- ts(window(us.rGDPg.q, start = 1994.00, end = end.date.exports),start=1994.00,frequency = 4)

us.impg.q <- ts(us.impg.q,start = 1980.00, frequency = 4)
us.impg.q.q.adj.length <- ts(window(us.impg.q, start = 1994.00, end = end.date.exports),start=1994.00,frequency = 4)




# ==============================================================
# 6) Differentiate between forecast vectors (for ARMAX) and normal data 
# We also calculate lagged variables
# ==============================================================      


# This section creates the forecast vectors used in the models. It creates a contemporary forecast vector,
# a lagged forecast vector, and the corresponding vectors with the normal data used in the estimation of the base model.
# The forecast vectors are needed in the ARMA-X models to forecast the exports.

# Also, for the price ranges, we need different vectors because the data for the growth rate for the price ranges only starts in the 
# second quarter of 1995.

# GDP World
world.rGDPg.q <- ts(world.rGDPg.q,start=1980.00,frequency = 4)
# create forecast vector
world.rGDPg.q.F <- ts(window(world.rGDPg.q,start=(end.date.exports+0.25),end=end.date.forecasts),start=(end.date.exports+0.25),frequency = 4)
# create lagged forecast vector
world.rGDPg.q.F.l1 <- ts(window(world.rGDPg.q,start= end.date.exports, end = end.date.forecasts-0.25 ),frequency = 4, start=end.date.exports+0.25)
# create the lagged vector to estimate the base model, data from 1994 until the date of the last entry of exports of Swiss watches
world.rGDPg.q.l1 <- ts(window(world.rGDPg.q,start=(1994.00-0.25),end=(end.date.exports-0.25)),start=1994.00,frequency = 4)
# vector for PRICE RANGES, LAGGED
world.rGDPg.PR.q.l1 <- ts(window(world.rGDPg.q,start=(1995.00),end=(end.date.exports-0.25)),start=1995.25,frequency = 4)




# GDP midE
midE.rGDPg.q <- ts(midE.rGDPg.q,start=1980.00,frequency = 4)
# create forecast vector
midE.rGDPg.q.F <- ts(window(midE.rGDPg.q,start=(end.date.exports+0.25),end=end.date.forecasts),start=(end.date.exports+0.25),frequency = 4)
# create lagged forecast vector
midE.rGDPg.q.F.l1 <- ts(window(midE.rGDPg.q,start= end.date.exports, end = end.date.forecasts-0.25 ),frequency = 4, start=end.date.exports+0.25)
# create the lagged vector to estimate the base model, data from 1994 until the date of the last entry of exports of Swiss watches
midE.rGDPg.q.l1 <- ts(window(midE.rGDPg.q,start=(1994.00-0.25),end=(end.date.exports-0.25)),start=1994.00,frequency = 4)
# vector for PRICE RANGES, LAGGED
midE.rGDPg.PR.q.l1 <- ts(window(midE.rGDPg.q,start=(1995.00),end=(end.date.exports-0.25)),start=1995.25,frequency = 4)



# GDP fe
fe.rGDPg.q <- ts(fe.rGDPg.q,start=1980.00,frequency = 4)
# create forecast vector
fe.rGDPg.q.F <- ts(window(fe.rGDPg.q,start=(end.date.exports+0.25),end=end.date.forecasts),start=(end.date.exports+0.25),frequency = 4)
# create lagged forecast vector
fe.rGDPg.q.F.l1 <- ts(window(fe.rGDPg.q,start= end.date.exports, end = end.date.forecasts-0.25 ),frequency = 4, start=end.date.exports+0.25)
# create the lagged vector to estimate the base model, data from 1994 until the date of the last entry of exports of Swiss watches
fe.rGDPg.q.l1 <- ts(window(fe.rGDPg.q,start=(1994.00-0.25),end=(end.date.exports-0.25)),start=1994.00,frequency = 4)
# vector for PRICE RANGES, LAGGED
fe.rGDPg.PR.q.l1 <- ts(window(fe.rGDPg.q,start=(1995.00),end=(end.date.exports-0.25)),start=1995.25,frequency = 4)



# GDP eu
eu.rGDPg.q <- ts(eu.rGDPg.q,start=1980.00,frequency = 4)
# create forecast vector
eu.rGDPg.q.F <- ts(window(eu.rGDPg.q,start=(end.date.exports+0.25),end=end.date.forecasts),start=(end.date.exports+0.25),frequency = 4)
# create lagged forecast vector
eu.rGDPg.q.F.l1 <- ts(window(eu.rGDPg.q,start= end.date.exports, end = end.date.forecasts-0.25 ),frequency = 4, start=end.date.exports+0.25)
# create the lagged vector to estimate the base model, data from 1994 until the date of the last entry of exports of Swiss watches
eu.rGDPg.q.l1 <- ts(window(eu.rGDPg.q,start=(1994.00-0.25),end=(end.date.exports-0.25)),start=1994.00,frequency = 4)
# vector for PRICE RANGES, LAGGED
eu.rGDPg.PR.q.l1 <- ts(window(eu.rGDPg.q,start=(1995.00),end=(end.date.exports-0.25)),start=1995.25,frequency = 4)


# GDP us
us.rGDPg.q <- ts(us.rGDPg.q,start=1980.00,frequency = 4)
# create forecast vector
us.rGDPg.q.F <- ts(window(us.rGDPg.q,start=(end.date.exports+0.25),end=end.date.forecasts),start=(end.date.exports+0.25),frequency = 4)
# create lagged forecast vector
us.rGDPg.q.F.l1 <- ts(window(us.rGDPg.q,start= end.date.exports, end = end.date.forecasts-0.25 ),frequency = 4, start=end.date.exports+0.25)
# create the lagged vector to estimate the base model, data from 1994 until the date of the last entry of exports of Swiss watches
us.rGDPg.q.l1 <- ts(window(us.rGDPg.q,start=(1994.00-0.25),end=(end.date.exports-0.25)),start=1994.00,frequency = 4)
# vector for PRICE RANGES, LAGGED
us.rGDPg.PR.q.l1 <- ts(window(us.rGDPg.q,start=(1995.00),end=(end.date.exports-0.25)),start=1995.25,frequency = 4)



# Imports CH
# create forecast vector:
ch.impg.q.F <- ts(window(ch.impg.q,start=(end.date.exports+0.25),end=end.date.forecasts),start=(end.date.exports+0.25),frequency = 4)
# create lagged forecast vector:
ch.impg.q.F.l1 <- ts(window(ch.impg.q,start= end.date.exports, end = end.date.forecasts-0.25 ),frequency = 4, start=end.date.exports+0.25)
# restrict the diffginal vector to data until 2017Q4
ch.impg.q.9417 <- ts(window(ch.impg.q,start=1994.00,end=end.date.exports),frequency = 4, start=1994.00)
ch.impg.q.l1 <- ts(window(ch.impg.q,start=(1994.00-0.25),end=(end.date.exports-0.25)),start=1994.00,frequency = 4)
# vector for PRICE RANGES, LAGGED
ch.impg.PR.q.l1 <- ts(window(ch.impg.q,start=(1995.00),end=(end.date.exports-0.25)),start=1995.25,frequency = 4)



# Exports CH
# create forecast vector:
ch.expg.q.F <- ts(window(ch.expg.q,start=(end.date.exports+0.25),end=end.date.forecasts),start=(end.date.exports+0.25),frequency = 4)
# create lagged forecast vector:
ch.expg.q.F.l1 <- ts(window(ch.expg.q,start= end.date.exports, end = end.date.forecasts-0.25 ),frequency = 4, start=end.date.exports+0.25)
# restrict the diffginal vector to data until 2017Q4
ch.expg.q.9417 <- ts(window(ch.expg.q,start=1994.00,end=end.date.exports),frequency = 4, start=1994.00)
ch.expg.q.l1 <- ts(window(ch.expg.q,start=(1994.00-0.25),end=(end.date.exports-0.25)),start=1994.00,frequency = 4)
# vector for PRICE RANGES, LAGGED
ch.expg.PR.q.l1 <- ts(window(ch.expg.q,start=(1995.00),end=(end.date.exports-0.25)),start=1995.25,frequency = 4)





# Imports us
# create forecast vector:
us.impg.q.F <- ts(window(us.impg.q,start=(end.date.exports+0.25),end=end.date.forecasts),start=(end.date.exports+0.25),frequency = 4)
# create lagged forecast vector:
us.impg.q.F.l1 <- ts(window(us.impg.q,start= end.date.exports, end = end.date.forecasts-0.25 ),frequency = 4, start=end.date.exports+0.25)
# restrict the diffginal vector to data until 2017Q4
us.impg.q.9417 <- ts(window(us.impg.q,start=1994.00,end=end.date.exports),frequency = 4, start=1994.00)
us.impg.q.l1 <- ts(window(us.impg.q,start=(1994.00-0.25),end=(end.date.exports-0.25)),start=1994.00,frequency = 4)
# vector for PRICE RANGES, LAGGED
us.impg.PR.q.l1 <- ts(window(us.impg.q,start=(1995.00),end=(end.date.exports-0.25)),start=1995.25,frequency = 4)



# Real Effective Exchange rate: One thing about the exchange rate is that it has no forecasts. we assume the best forecast
# is the last value itself.
ch.rfx.q.l1 <- c(NA,ch.rfx.q)
ch.rfx.q.l1 <- ts(ch.rfx.q.l1[1:(length(ch.rfx.q.l1)-1)],frequency = 4, start = 1994.00)

# for the exchange rate, the best forecast is just its last value. That we can use it in the ARMAX
# models, we will include this assumption.
ch.rfx.q.F <- ts(c(rep(ch.rfx.q[length(ch.rfx.q)],6)),start = (end.date.exports+0.25), frequency = 4)
ch.rfx.q.F.l1 <- ts(c(ch.rfx.q[(length(ch.rfx.q)-1)],rep(ch.rfx.q[length(ch.rfx.q)],5)), start=(end.date.exports+0.25),frequency = 4)

# vector for PRICE RANGES, LAGGED. Price ranges just start in 1995, second quarter.
ch.rfx.PR.q.l1 <- suppressWarnings(ts(window(ch.rfx.q.l1,start=(1995.25),end=(end.date.exports)),start=1995.25,frequency = 4))
ch.rfx.PR.q.F.l1 <- ch.rfx.q.F.l1

# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::      
#                                                             END OF THE CODE      
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::      
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  