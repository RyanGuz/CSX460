### Last assignment to get the fit .rds


library(ggplot2)
library(dplyr)
library(readr)
library(magrittr)
library(lubridate)
library(stringr)
library(data.table)

flights <- read_csv("data/flights.csv")
planes   <- read_csv("data/planes.csv")
airports <- read_csv("data/airports.csv") 
weather  <- read_csv("data/weather.csv")

flightsDT  <- fread("data/flights.csv") 
planesDT   <- fread("data/planes.csv")  %>% setkey(tailnum)
airportsDT <- fread("data/airports.csv")  %>% setkey(faa) 
weatherDT  <- fread("data/weather.csv") %>%   setkey(origin,year,month,day,hour)

YX <- flightsDT 
YX %<>% merge( planesDT, all.x = TRUE, by='tailnum', suffixes=c('','.pl') )
YX %<>% merge( weatherDT, all.x = TRUE, by=c('origin','year','month','day','hour'), suffixes=c('','.we') )
YX %<>% merge( airportsDT, all.x = TRUE, by.x='origin', by.y='faa', suffixes=c('','.orig') )
YX %<>% merge( airportsDT, all.x = TRUE, by.x='dest', by.y='faa', suffixes=c('','.dest') )

YX <- YX[ , late := factor(arr_delay >= 22) ]

yx <- copy(YX)

form <- 
  late ~ dep_delay + month + air_time + distance + carrier + lat.dest + lon.dest

fit.glm <- glm( form, yx, family=binomial() )
fit.step <- MASS::stepAIC(fit.glm, scope=list(upper=. ~ ., lower = . ~ 1 ) )
summary(fit.step)
saveRDS(fit.step, file="fit.step.rds")


preds <-  predict( fit.step, yx, type="response")
noData <- preds %>% is.na()

yhat <- preds[!noData]
yhat <- ifelse(yhat>=.5, TRUE, FALSE)
y <- yx$late[!noData]
  
logitModel <- data.frame(y, yhat)
saveRDS(logitModel, file="logitModel.rds")

