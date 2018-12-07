setwd("B:/Project/Pilot project & Pecan Competition/4. Raw Data for R")

weather.raw <- read.csv('weather raw.csv')

weather.raw$CDH.dry <- with(weather.raw, ifelse(Tdry>=75, Tdry-75, 0))
weather.raw$CDH.wet <- with(weather.raw, ifelse(Twet>=75, Twet-75, 0))
weather.raw$HDH.dry <- with(weather.raw, ifelse(Tdry<65, 65-Tdry, 0))
weather.raw$HDH.wet <- with(weather.raw, ifelse(Twet<65, 65-Twet, 0))
weather.raw$HHH.60 <- with(weather.raw, ifelse(humidity>0.6, humidity-0.6, 0))
weather.raw$LHH.30 <- with(weather.raw, ifelse(humidity<0.3, 0.3-humidity, 0))
weather.raw$HWH.7.5<- with(weather.raw, ifelse(wind_speed>7.5,wind_speed-7.5,0))
weather.raw$LWH.7.5<- with(weather.raw, ifelse(wind_speed<7.5,7.5-wind_speed,0))
weather.raw$LWH.3.5<- with(weather.raw, ifelse(wind_speed<3.5,3.5-wind_speed,0))

weather.temp <- weather.raw[,c(10:12,18:26)] # data with only monthly and variables data

library(data.table)
weather <- setDT(weather.temp)[, lapply(.SD, sum), by = .(city,year,month)] ## Convert daily data into Monthly

rm(weather.temp)

########################################################################################
setwd("B:/Project/Pilot project & Pecan Competition/4. Raw Data for R")

energy.raw <- read.csv('energy raw.csv')

energy.raw[is.na(energy.raw)] <- 0
energy.raw$Cooling <- energy.raw$Air1 + energy.raw$Air2 + energy.raw$Air3 + energy.raw$Airwindow
energy.raw$Heating <- energy.raw$Heater + energy.raw$Furnace1 + energy.raw$Furnace2

heating.raw <- energy.raw[,c(1,3,4,5,14)]

library(data.table)
heating.1 <- setDT(heating.raw)[, lapply(.SD, sum), by = .(year,dataid,month)] ## Convert daily data into Monthly

a <- subset(heating.1, (heating.1$Heating >= heating.1$Use)) # variables with Use < Cooling (Faulty observations)

heating.1 <- heating.1[!(heating.1$Heating >= heating.1$Use),] #remove observations with Cooling > Use

metadata.raw <- read.csv('metadata.csv')
metadata <- metadata.raw[,c(1,11,16,17)]

colnames(metadata)[colnames(metadata)=="total_square_footage"] <- "Area"
rm(metadata.raw)

heating.2  <- merge(metadata, heating.1, by = 'dataid', all = TRUE) #all=TRUE will trigger other observations from metadata
summary(heating.1)
summary(heating.2)

heating.2 <- heating.2[complete.cases(heating.2[ , 4:8]),] #Remove extra observations from metadata & obs with NA "Area" #Excempt Year of const
#NA's of Columns mentioned will be removed

heating.2$Age <- heating.2$year-heating.2$house_construction_year
heating.2 <- heating.2[,c(1:2,4:9)]

index_NA <- which(is.na(heating.2$Age))
heating.2$Age[index_NA] <- median(heating.2$Age, na.rm = TRUE)

heating.2$city <- gsub("Round Rock", "Austin", heating.2$city)
heating.2$city <- gsub("Bee Cave", "Austin", heating.2$city)

heating.3  <- merge(heating.2, weather, by = c("city", "year", "month"), all = TRUE) #all=TRUE will trigger other observations from metadata
summary(heating.3)

heating.3 <- heating.3[complete.cases(heating.3[ , 8:9]),] #Remove extra observations from metadata & obs with NA "Area" #Excempt Year of const
#NA's of Columns mentioned will be removed

gas <- read.csv('Gas usage.csv')
heating.4 <- merge(heating.3, gas, by = c("year", "month", "dataid"), all=TRUE)
summary(heating.4)

###Replace NAs in Gas with 0s
heating.4$gas_usage[is.na(heating.4$gas_usage)] <- 0
heating.4 <- heating.4[complete.cases(heating.4[ , 17]),] #Remove extra observations from metadata & obs with NA "Area" #Excempt Year of const
#NA's of Columns mentioned will be removed

heating.4$gas_kwh <- heating.4$gas_usage*0.29307
heating.4$total.heat <- heating.4$gas_kwh + heating.4$Heating
heating.4$total.use <- heating.4$gas_kwh + heating.4$Use

write.csv('heating.csv', x=heating.4)
heating.winter <- subset(heating.4, month %in% c('Jan', 'Feb', 'Nov', 'Dec')) #df with summer months
summary(heating.winter)

library(ggplot2)
ggplot(data=heating.winter, aes(x=total.use, y=total.heat, color = city)) + geom_point() + geom_smooth(method = lm) + facet_wrap(~ month + year)

heating.winter$total.heating.sqft <- heating.winter$total.heat/heating.winter$Area
heating.winter$total.use.sqft <- heating.winter$total.use/heating.winter$Area

heating.winter <- heating.winter[apply(heating.winter[c(22:23)],1,function(z) any(z!=0)),] ## If both total heat and total use are zero
heating.winter <- heating.winter[apply(heating.winter[c(22:23)],1,function(z) !any(z==0)),] ## If any of total heat or total use are zero

heating.winter$Month <- sapply(heating.winter$month,function(x) grep(paste("(?i)",x,sep=""),month.abb))
summary(heating.winter)

write.csv('heating.winter.csv', x=heating.winter)

correlation.table <- cor(heating.winter[,c(21:22,5,8:17)])

normal.h.dry <- heating.winter[,c(22:24,5,8:9,11,13:17)]
normal.h.wet <- heating.winter[,c(22:24,5,8,10,12:17)]
View(correlation.table)

rm(heating.1,heating.2,heating.3,heating.4, heating.raw,weather.raw,metadata)

hist(heating.winter$total.heating.sqft)
hist(heating.winter$use.sqft)

##
heat.check <- heating.winter[,c(1:4,6:7,19)]
write.csv('heat.check.csv', x=heat.check)

normal.heating.dry <- do.call(poly, c(lapply(2:11, function(x) normal.h.dry[,x]), degree=2, raw=T))

#####################################################################################
## Cooling
cooling.raw <- energy.raw[,c(1,4,3,5,13)]

library(data.table)
cooling.1 <- setDT(cooling.raw)[, lapply(.SD, sum), by = .(year,dataid,month)] ## Convert daily data into Monthly

b <- subset(cooling.1, (cooling.1$Cooling >= cooling.1$Use)) # variables with Use < Cooling (Faulty observations)
cooling.1 <- cooling.1[!(cooling.1$Cooling >= cooling.1$Use),] #remove observations with Cooling > Use

metadata.raw <- read.csv('metadata.csv')
metadata <- metadata.raw[,c(1,11,16,17)]

colnames(metadata)[colnames(metadata)=="total_square_footage"] <- "Area"
rm(metadata.raw)

cooling.2  <- merge(metadata, cooling.1, by = 'dataid', all = TRUE) #all=TRUE will trigger other observations from metadata
summary(cooling.1)
summary(cooling.2)

cooling.2 <- cooling.2[complete.cases(cooling.2[ , 4:8]),] #Remove extra observations from metadata & obs with NA "Area" #Excempt Year of const
#NA's of Columns mentioned will be removed

cooling.2$Age <- cooling.2$year-cooling.2$house_construction_year
cooling.2 <- cooling.2[,c(1:2,4:9)]

index_NA <- which(is.na(cooling.2$Age))
cooling.2$Age[index_NA] <- median(cooling.2$Age, na.rm = TRUE)

cooling.2$city <- gsub("Round Rock", "Austin", cooling.2$city)
cooling.2$city <- gsub("Bee Cave", "Austin", cooling.2$city)

cooling.3  <- merge(cooling.2, weather, by = c("city", "year", "month"), all = TRUE) #all=TRUE will trigger other observations from metadata


cooling.3 <- cooling.3[complete.cases(cooling.3[ , 9:10]),] #Remove extra observations from metadata & obs with NA "Area" #Excempt Year of const
#NA's of Columns mentioned will be removed

write.csv('cooling.csv', x=cooling.3)

cooling.summer <- subset(cooling.3, month %in% c('May','Jun','Jul','Aug','Sep')) #df with summer months

library(dplyr)
cooling.summer <- cooling.summer %>% filter(Cooling != 0)

cooling.summer$cooling.sqft <- cooling.summer$Cooling/cooling.summer$Area
cooling.summer$use.sqft <- cooling.summer$Use/cooling.summer$Area

cooling.summer$Month <- sapply(cooling.summer$month,function(x) grep(paste("(?i)",x,sep=""),month.abb))

summary(cooling.summer)
write.csv('cooling.summer.csv', x=cooling.summer)

correlation.table <- cor(cooling.summer[,c(18:19,5,8:17)])

normal.dry <- cooling.summer[,c(18:19,5,8,9,11,13:17)]
normal.wet <- cooling.summer[,c(18:19,5,8,10,12:17)]

rm(cooling.1,cooling.2,cooling.3,cooling.raw,weather.raw,metadata)

hist(cooling.summer$cooling.sqft)
hist(cooling.summer$use.sqft)

normal.cooling.dry <- do.call(poly, c(lapply(2:11, function(x) normal.dry[,x]), degree=2, raw=T))

