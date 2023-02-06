#Packages
install.packages("readr")
install.packages("tidyverse")
install.packages("stringr")
install.packages("plyr")
install.packages("dplyr")
install.packages("ggmap")
install.packages("tidyr")
install.packages("geobr")
install.packages("ggplot2")
install.packages("sf")
install.packages("sqldf")
install.packages("openxlsx")
install.packages("viridis")
install.packages("knitr")
install.packages("rgdal")
install.packages("ggmap")
install.packages("scales")
install.packages("rmapshaper")



#Libraries
library(readr)
library(tidyverse)
library(stringr)
library(plyr)
library(dplyr)
library(ggmap)
library(tidyr)
library(geobr)
library(ggplot2)
library(sf)
library(sqldf)
library(openxlsx)
library(viridis)
library(knitr)
library(rgdal)# R wrapper around GDAL/OGR
library(ggmap)
library(scales)
library(rmapshaper)


#Creates a list of files which is equivalent to their station name 
file_names <- list.files("Data", pattern="*.csv", full.names = FALSE)
view(file_names)

#Combines all files data
weather_data <- lapply(file_names, function(x) {
  a <- paste0("Data/", x)
  BrazilWeatherData <- read_csv2(a, skip = 10)
  cbind(station = x, BrazilWeatherData)
})

#Turns large list into dataframe
BrazilWeather <- rbind.fill(weather_data)
#Removes weather_data data that is no longer needed
rm(weather_data)
rm(file_names)
rm(BrazilWeather)

#Removes columns not needed
BrazilWeather <- BrazilWeather[, -c(11:12)]

#Renames Columns
colnames(BrazilWeather)
colnames(BrazilWeather) <- c('Station', 'Date', 'Evaporation', 'Total_Insolation', 'Precipitation', 'Max_Temperature', 'Avg_Temperature', 'Min_Temperature', 'Humidity', 'Wind_Speed')

#Removes unnecessary info from stations column
BrazilWeather$Station <- gsub(".csv", "", BrazilWeather$Station)

#Replaces commas into periods and changes them to numeric
BrazilWeather[-1:-2] <- data.frame(lapply(BrazilWeather[-1:-2], function(x) {
  as.numeric(gsub(",", ".", x))
  }))

#Converts Date from character to date
BrazilWeather$Date <- as.Date(BrazilWeather$Date)
str(BrazilWeather)
#Backup to Brazil Weather Station
BackupBW <- BrazilWeather
BrazilWeather <- BackupBW

#Import Weather Station Data
WeatherStations <- read.csv("Weather Stations/WeatherStations.csv")

#Merge Brazil Weather with Weather Stations
BrazilWeather <- merge(BrazilWeather, WeatherStations, by = "Station", all = FALSE)
rm(WeatherStations)

#returns stations with null values across all columns
nullStations <- BrazilWeather[rowSums(is.na(BrazilWeather)) > 6, ]
nullStationsCount <- data.frame(tapply(nullStations$Date, nullStations$Station, length))
colnames(nullStationsCount) <- "Null Rows By Station"
nullStationsCount$Station <- row.names(nullStationsCount)
BrazilWeather <- merge(BrazilWeather, nullStationsCount, by = "Station", all = TRUE)
rm(nullStations)
rm(nullStationsCount)

#Shows unique stations before eliminating NULL rows
uniqueStationsBefore <- data.frame(unique(BrazilWeather$Station))

#Removes stations with more than 365 days of missing rows
BrazilWeather <-BrazilWeather[BrazilWeather$`Null Rows By Station` <= 365, ]
#Removes the rows that have blank date
BrazilWeather <- BrazilWeather[!is.na(BrazilWeather$Date), ]

#Shows unique stations after eliminating NULL rows
uniqueStationsAfter <- data.frame(unique(BrazilWeather$Station))
rm(uniqueStationsBefore)
rm(uniqueStationsAfter)

#Creates Avg Temp for those rows  with NAs by Averaging the Max and Min temp for the day
BrazilWeather$Avg_Temperature_Calculated <- (BrazilWeather$`Max_Temperature` + BrazilWeather$`Min_Temperature`)/2
BrazilWeather$`Avg_Temperature`[is.na(BrazilWeather$`Avg_Temperature`)] <- BrazilWeather$Avg_Temperature_Calculated[is.na(BrazilWeather$`Avg_Temperature`)]

#Removes columns no longer needed
BrazilWeather <- BrazilWeather[, -c(15:16)]


#must separate out month from the date to fill NAs
BrazilWeather <- separate(BrazilWeather, Date, sep="-", into = c("Year", "Month", "Day"))

#Replace NAs with Medians by group
#plyr causes problems moving forward
detach(package:plyr)

#Summarizes Each Monthly to Replace NAs
BrazilWeather <- BrazilWeather %>% 
  group_by(Station, Year, Month) %>%
  mutate(
    across(c('Evaporation', 'Total_Insolation', 'Precipitation', 'Max_Temperature', 'Avg_Temperature', 'Min_Temperature', 'Humidity', 'Wind_Speed'), function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
  )

#Creates backup to this point
BackupBW <- BrazilWeather

#Make Summarized Monthly Table of Data by State
monthlySummary <- BrazilWeather %>% group_by(State, Year, Month) %>% summarise_all(list(~ mean(.x, na.rm = TRUE)))
#Removes Unnecessary Columns
monthlySummary <- monthlySummary[, -c(4:5)]
#Write to CSV
write.csv(monthlySummary, "monthlySummary.csv")

#Import Firespots Data
Firespots <- read.csv("Firespots/Firespots.csv")
Firespots <- Firespots[,-c(3,5,6)]
colnames(Firespots) <- c("Year","Month","State","Firespots")
#Brazil Months as integer
monthlySummary$Month <- as.integer(monthlySummary$Month)

#Merge Firespots with Monthly Summary
monthlySummary <- merge(monthlySummary, Firespots, by = c("State","Month", "Year"), all = TRUE)
rm(Firespots)

#Clean Monthly Summary
nullStations <- monthlySummary[rowSums(is.na(monthlySummary)) > 7, ]
unique(nullStations$State)
#Remove null stations from monthlySummary
monthlySummary <- monthlySummary[monthlySummary$State != "AP",]
monthlySummary <- monthlySummary[monthlySummary$State != "MS",]
monthlySummary <- monthlySummary[monthlySummary$State != "RO",]
#Remove nullStations
rm(nullStations)
#Write to CSV
write.csv(monthlySummary, "monthlySummary.csv")

#Make Summarized Yearly Table of Data by State
yearlySummary <- monthlySummary %>% group_by(State, Year) %>% summarise_all(list(~ mean(.x, na.rm = TRUE)))
#Removes Unnecessary Columns
yearlySummary <- yearlySummary[, -c(3)]

#Import Deoforestation Data
Deforestation <- read.csv("Deforestation/Deforestation.csv")
Deforestation <- Deforestation[, -c(3)]
colnames(Deforestation) <- c("Year","Deforestation","State")

#Merge Deforestation with Yearly Summary
yearlySummary <- merge(yearlySummary, Deforestation, by = c("State", "Year"), all = FALSE)
yearlySummary$Deforestation <- as.numeric(gsub(",", "", yearlySummary$Deforestation))
rm(Deforestation)

#create cumulative deforestation column
yearlySummary <- yearlySummary %>% group_by(State) %>% mutate("cumDeforestation" = cumsum(Deforestation))


#Write to CSV
write.csv(yearlySummary, "yearlySummary.csv")

#Start of Summary Analysis
install.packages("ggcorrplot")
library(ggcorrplot)

#Run Correlations for Monthly Summary All
corrMonth <- round(cor(monthlySummary[,c(4,5,6,7,8,9,10,11,14,15)], use = "complete.obs"), 2)
ggcorrplot(corrMonth, hc.order = FALSE, type = 'lower', lab = TRUE) +
  ggtitle("Monthly Summary Correlation Matrix")



#Run Correlations for Yearly Summary All
corrYear <- round(cor(yearlySummary[,c(3,4,5,6,7,8,9,10,13,14,15)], use = "complete.obs"), 2)
ggcorrplot(corrYear, hc.order = FALSE, type = 'lower', lab = TRUE) +
  ggtitle("Yearly Summary Correlation Matrix")

#Run Correlations for Yearly Summary All with cumulative deforestation
corrYear <- round(cor(yearlySummary[,c(3,4,5,6,7,8,9,10,13,14,16)], use = "complete.obs"), 2)
ggcorrplot(corrYear, hc.order = FALSE, type = 'lower', lab = TRUE) +
  ggtitle("Yearly Summary Correlation Matrix")

#Reduce yearly and monthly summaries to only Mato Grosso, Para, Amazonas, and Tocantins
monthlySummaryRefined <- monthlySummary[monthlySummary$State %in% c('PA', 'MT', 'AM', 'TO'), ]
yearlySummaryRefined <- yearlySummary[yearlySummary$State %in% c('PA', 'MT', 'AM', 'TO'), ]

#Run Correlations for Monthly Summary Refined by states we are focusing on 
corrMonthRefined <- round(cor(monthlySummaryRefined[,c(4,5,6,7,8,9,10,11,14,15)], use = "complete.obs"), 2)
ggcorrplot(corrMonthRefined, hc.order = FALSE, type = 'lower', lab = TRUE) +
  ggtitle("Monthly Summary Correlation Matrix Refined")

#Run Correlations for Yearly Summary Refined
corrYearRefined <- round(cor(yearlySummaryRefined[,c(3,4,5,6,7,8,9,10,13,14,15)], use = "complete.obs"), 2)
ggcorrplot(corrYearRefined, hc.order = FALSE, type = 'lower', lab = TRUE) +
  ggtitle("Yearly Summary Correlation Matrix Refined")

#Run Correlations for Yearly Summary Refined with Cumulative Deforestation
corrYearRefined <- round(cor(yearlySummaryRefined[,c(3,4,5,6,7,8,9,10,13,14,16)], use = "complete.obs"), 2)
ggcorrplot(corrYearRefined, hc.order = FALSE, type = 'lower', lab = TRUE) +
  ggtitle("Yearly Summary Correlation Matrix Refined")

#Regression Analysis -----------------------------------------------------------

#Original Deforestation Regression
deforestationRegression <- lm(formula=Deforestation ~ Total_Insolation + Evaporation + Precipitation + Max_Temperature + Avg_Temperature + Min_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(deforestationRegression)
#Minus Total_Insolation
deforestationRegression <- lm(formula=Deforestation ~ Evaporation + Precipitation + Max_Temperature + Avg_Temperature + Min_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(deforestationRegression)
#Minus Min_temp
deforestationRegression <- lm(formula=Deforestation ~ Evaporation + Precipitation + Max_Temperature + Avg_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(deforestationRegression)
#Minus Avg_Temp
deforestationRegression <- lm(formula=Deforestation ~ Evaporation + Precipitation + Max_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(deforestationRegression)
#Minus Evapoaration
deforestationRegression <- lm(formula=Deforestation ~ Precipitation + Max_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(deforestationRegression)
#Minus Precipitation
deforestationRegression <- lm(formula=Deforestation ~ Max_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(deforestationRegression)
#Minus Max_Temp - All variables significant
deforestationRegression <- lm(formula=Deforestation ~ Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(deforestationRegression)




#Cumulative Deforestation Regression
summary(lm(formula=cumDeforestation ~ Total_Insolation + Evaporation + Precipitation + Max_Temperature + Avg_Temperature + Min_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummary))

#Minus Average Temp
summary(lm(formula=cumDeforestation ~ Total_Insolation + Evaporation + Precipitation + Max_Temperature + Min_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummary))

#Minus Max Temp
summary(lm(formula=cumDeforestation ~ Total_Insolation + Evaporation + Precipitation + Min_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummary))

#Cumulative Deforestation Regression with Max and Average Temp
summary(lm(formula=cumDeforestation ~ Max_Temperature, data=yearlySummary))
summary(lm(formula=cumDeforestation ~ Avg_Temperature, data=yearlySummary))



#Original Max_Temp Regression
maxtempRegression <- lm(formula=Max_Temperature ~ Total_Insolation + Evaporation + Precipitation + Deforestation + Avg_Temperature + Min_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(maxtempRegression)
#Minus Evaporation
maxtempRegression <- lm(formula=Max_Temperature ~ Total_Insolation + Precipitation + Deforestation + Avg_Temperature + Min_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(maxtempRegression)
#Minus Deforestation
maxtempRegression <- lm(formula=Max_Temperature ~ Total_Insolation + Precipitation + Avg_Temperature + Min_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(maxtempRegression)
#Minus Precipitation
maxtempRegression <- lm(formula=Max_Temperature ~ Total_Insolation + Avg_Temperature + Min_Temperature + Humidity + Wind_Speed + Altitude + Firespots, data=yearlySummaryRefined)
summary(maxtempRegression)
#Minus Altitude
maxtempRegression <- lm(formula=Max_Temperature ~ Total_Insolation + Avg_Temperature + Min_Temperature + Humidity + Wind_Speed + Firespots, data=yearlySummaryRefined)
summary(maxtempRegression)


#Trend Charts ------------------------------------------
install.packages("ggpmisc")
library(ggpmisc)

#Humidity Trend Chart
humidityplot <- ggplot(yearlySummaryRefined, aes(x=Year, y=Humidity, group=State, color = State)) + geom_line() + xlab("Year") + ylab("Average Humidity") + ggtitle("Average Humidity by State from 1999-2019") + scale_fill_discrete(name = "State") + geom_smooth(method = "lm", se = FALSE)
humidityplot
summary(humidityplot)

#With Regression equation:
HumidityPlot <- ggplot(yearlySummaryRefined, aes(x=Year, y=Humidity, group=State, color = State)) + geom_line()+ geom_smooth(method = "lm", se = FALSE) + stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)

#Wind Trend Chart
windplot <- ggplot(yearlySummaryRefined, aes(x=Year, y=Wind_Speed, group=State, color = State)) + geom_line() + xlab("Year") + ylab("Average Wind Speed") + ggtitle("Average Windspeed by State from 1999-2019") + scale_fill_discrete(name = "State") + geom_smooth(method = "lm", se = FALSE)
windplot

#With Regression Equation
#WindSpeed
WindSpeedPlot <- ggplot(yearlySummaryRefined, aes(x=Year, y=Wind_Speed, group=State, color = State)) + geom_line()+ geom_smooth(method = "lm", se = FALSE) + stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)

#Firespots Trend Chart
firespotsplot <- ggplot(yearlySummaryRefined, aes(x=Year, y=Firespots, group=State, color = State)) + geom_line() + xlab("Year") + ylab("Average Firespots") + ggtitle("Average Firespots by State from 1999-2019") + scale_fill_discrete(name = "State") + geom_smooth(method = "lm", se = FALSE)
firespotsplot

#With regression equation
firespotsplot <- ggplot(yearlySummaryRefined, aes(x=Year, y=Firespots, group=State, color = State)) + geom_line()+ geom_smooth(method = "lm", se = FALSE) + stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)

#Deforestation Trend Chart
deforestationplot <- ggplot(yearlySummaryRefined, aes(x=Year, y=Deforestation, group=State, color = State)) + geom_line() + xlab("Year") + ylab("Deforestation") + ggtitle("Deforestation by State from 1999-2019") + scale_fill_discrete(name = "State") + geom_smooth(method = "lm", se = FALSE)
deforestationplot

#With regression equation
deforestationplot <- ggplot(yearlySummaryRefined, aes(x=Year, y=Deforestation, group=State, color = State)) + geom_line()+ geom_smooth(method = "lm", se = FALSE) + stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)

#Deforestation Cumulative Trend Chart
cumdeforestationplot <- ggplot(yearlySummaryRefined, aes(x=Year, y=cumDeforestation, group=State, color = State)) + geom_line() + xlab("Year") + ylab("Deforestation") + ggtitle("Cumulative Deforestation by State from 1999-2019") + scale_fill_discrete(name = "State")
cumdeforestationplot

#With regression equation
cumdeforestationplot <- ggplot(yearlySummaryRefined, aes(x=Year, y=cumDeforestation, group=State, color = State)) + geom_line()+ geom_smooth(method = "lm", se = FALSE) + stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE)


#Altitude Trend Chart
altitudeplot <- ggplot(yearlySummaryRefined, aes(x=State, y=Altitude, fill = State)) + geom_col() + xlab("Year") + ylab("Altitude") + ggtitle("Average Altitude by State") + scale_fill_discrete(name = "State")
altitudeplot



# Create a dataframe to store max temperature by year and state
dfMaxTemp <- aggregate(x=yearlySummary[,7], by=list(yearlySummary$Year,yearlySummary$State), FUN="mean")
colnames(dfMaxTemp) <- c("Year","State","Max_Temperature")
dfMaxTemp

# Create a dataframe to store max temperature by state
dfMaxTempState <- aggregate(dfMaxTemp$Max_Temperature, by=list(State=dfMaxTemp$State), FUN="mean")
colnames(dfMaxTempState) <- c("State","Max_Temperature")
dfMaxTempState

# Create a dataframe to store firespots by year and state
dfFirespots <- aggregate(yearlySummaryRefined$Firespots, by=list(yearlySummaryRefined$Year, yearlySummaryRefined$State), FUN="mean")
colnames(dfFirespots) <- c("Year","State","Firespots")
dfFirespots

# Create a dataframe to store deforestation by year and state
dfDeforestation <- aggregate(yearlySummaryRefined$Deforestation, by=list(yearlySummaryRefined$Year, yearlySummaryRefined$State), FUN="mean")
colnames(dfDeforestation) <- c("Year","State","Deforestation")
dfDeforestation

# Create a dataframe to store firespots by year and state for all states
dfFirespotsAll <- aggregate(yearlySummary$Firespots, by=list(yearlySummary$Year, yearlySummary$State), FUN="mean")
colnames(dfFirespotsAll) <- c("Year","State","Firespots")
dfFirespotsAll

# Create a dataframe to store deforestation by year and state for all states
dfDeforestationAll <- aggregate(yearlySummary$Deforestation, by=list(yearlySummary$Year, yearlySummary$State), FUN="mean")
colnames(dfDeforestationAll) <- c("Year","State","Deforestation")
dfDeforestationAll

# Create a dataframe to store max temperature by year and state for refined states
dfMaxTempRefined <- aggregate(x=yearlySummaryRefined[,7], by=list(yearlySummaryRefined$Year,yearlySummaryRefined$State), FUN="mean")
colnames(dfMaxTempRefined) <- c("Year","State","Max_Temperature")
dfMaxTempRefined

# Create a dataframe to store avg temperature by year and state
dfAvgTemp <- aggregate(x=yearlySummary[,8], by=list(yearlySummary$Year,yearlySummary$State), FUN="mean")
colnames(dfAvgTemp) <- c("Year","State","Avg_Temperature")
dfAvgTemp

# Create a dataframe to store avg temperature by state
dfAvgTempState <- aggregate(dfAvgTemp$Avg_Temperature, by=list(State=dfAvgTemp$State), FUN="mean")
colnames(dfAvgTempState) <- c("State","Avg_Temperature")
dfAvgTempState

# Create a dataframe to store avg temperature by year and state for refined states
dfAvgTempRefined <- aggregate(x=yearlySummaryRefined[,8], by=list(yearlySummaryRefined$Year,yearlySummaryRefined$State), FUN="mean")
colnames(dfAvgTempRefined) <- c("Year","State","Avg_Temperature")
dfAvgTempRefined

# Create a plot of max temp by year and state for all states
g <- ggplot(dfMaxTemp, aes(x=Year, y=Max_Temperature, group=State, color = State)) + geom_line()
g + xlab("Year") + ylab("Max Temperature") + ggtitle("Max Temperature by State from 1999-2019") + scale_fill_discrete(name = "State") + theme(plot.title = element_text(hjust = 0.5))

# Create a plot of max temp by year and state for refined states
g <- ggplot(dfMaxTempRefined, aes(x=Year, y=Max_Temperature, group=State, color = State)) + geom_line()
g + geom_smooth(method='lm', formula= y~x, se=FALSE) +  ggtitle("Max Temperature by State from 1999-2019") + theme(plot.title = element_text(hjust = 0.5))

# Create a plot of avg temp by year and state for all states
g <- ggplot(dfAvgTemp, aes(x=Year, y=Avg_Temperature, group=State, color = State)) + geom_line()
g + xlab("Year") + ylab("Avg Temperature") + ggtitle("Avg Temperature by State from 1999-2019") + scale_fill_discrete(name = "State") + theme(plot.title = element_text(hjust = 0.5))

# Create a plot of avg temp by year and state for refined states
g <- ggplot(dfAvgTempRefined, aes(x=Year, y=Avg_Temperature, group=State, color = State)) + geom_line()
g + geom_smooth(method='lm', formula= y~x, se=FALSE) +  ggtitle("Avg Temperature by State from 1999-2019") + theme(plot.title = element_text(hjust = 0.5))

# Create a plot of firespots by year and state for refined states
g <- ggplot(dfFirespots, aes(x=Year, y=Firespots, group=State, color = State)) + geom_line()
g + geom_smooth(method='lm', formula= y~x, se=FALSE) +  ggtitle("Average Firespots by State from 1999-2019") + theme(plot.title = element_text(hjust = 0.5))

# Create a plot of deforestation by year and state for refined states
g <- ggplot(dfDeforestation, aes(x=Year, y=Deforestation, group=State, color = State)) + geom_line()
g + geom_smooth(method='lm', formula= y~x, se=FALSE) +  ggtitle("Average Deforestation by State from 1999-2019") + theme(plot.title = element_text(hjust = 0.5))

# Create a scatter plot of firespots overtime for entire dataset
plot(dfYearlyFirespots$Year, dfYearlyFirespots$AverageFirespots,
     main="Firespots from 1999-2019",
     xlab="Years",
     ylab="Firespots",
     col="blue",
     type="l",
     xaxp = c(1999, 2019, 20),
     cex.axis=0.5)

# Create plot of average firespots by state for all states
g <- ggplot(dfYearlyFirespotsState, aes(x=Year, y=AverageFirespots, group=State, color = State)) + geom_line()
g + xlab("Year") + ylab("Average Firespots") + ggtitle("Average Firespots by State from 1999-2019") + scale_fill_discrete(name = "State")

# Create a dataframe to store firespots by year
dfYearlyFirespots <- aggregate(x=yearlySummary[,15], by=list(yearlySummary$Year), FUN="mean")
colnames(dfYearlyFirespots) <- c("Year","AverageFirespots")
dfYearlyFirespots

# Create a dataframe to store firespots by state
dfStatesFirespots <- aggregate(x=yearlySummary[,15], by=list(yearlySummary$State), FUN="mean")
colnames(dfStatesFirespots) <- c("State","AverageFirespots")
dfStatesFirespots[order(-dfStatesFirespots$AverageFirespots),]

# Create a dataframe to store firespots by year and state
dfYearlyFirespotsState <- aggregate(x=yearlySummary[,15], by=list(yearlySummary$Year,yearlySummary$State), FUN="mean")
colnames(dfYearlyFirespotsState) <- c("Year","State","AverageFirespots")
dfYearlyFirespotsState

# Create a scatter plot of firespots overtime for entire dataset
plot(dfYearlyFirespots$Year, dfYearlyFirespots$AverageFirespots,
     main="Firespots from 1999-2019",
     xlab="Years",
     ylab="Firespots",
     col="blue",
     type="l",
     xaxp = c(1999, 2019, 20),
     cex.axis=0.5)

# Create a bar plot of firespots by state
barplot(dfStatesFirespots$AverageFirespots,names.arg=dfStatesFirespots$State,
        main="Firespots by State",
        ylab="Firespots",
        col=rainbow(20),
        las=2,
        cex.names=0.5)

# Find highest firespots
maxFirespot = max(dfFirespotsAll$Firespots)
maxFirespot

# Find the index with highest firespots
index = as.numeric(rownames(dfFirespotsAll)[which.max(dfFirespotsAll$Firespots)])
index

# Find the year and state with highest firespots
paste("The highest firespots occurred in", dfFirespotsAll$State[index], "in the year", dfFirespotsAll$Year[index])

# Order data frame by highest firespots in descending order
dfFirespotsAll[order(-dfFirespotsAll$Firespots),]

# Create a dataframe to store deforestation by year
dfYearlyDeforestation <- aggregate(x=yearlySummary[,16], by=list(yearlySummary$Year), FUN="mean")
colnames(dfYearlyDeforestation) <- c("Year","AverageDeforestation")
dfYearlyDeforestation$Year <- as.numeric(dfYearlyDeforestation$Year)

# Create a dataframe to store deforestation by state
dfStatesDeforestation <- aggregate(x=yearlySummary[,16], by=list(yearlySummary$State), FUN="mean")
colnames(dfStatesDeforestation) <- c("State","AverageDeforestation")
dfStatesDeforestation[order(-dfStatesDeforestation$AverageDeforestation),]

# Create a dataframe to store deforestation by year and state
dfYearlyDeforestationState <- aggregate(x=yearlySummary[,16], by=list(yearlySummary$Year,yearlySummary$State), FUN="mean")
colnames(dfYearlyDeforestationState) <- c("Year","State","AverageDeforestation")
dfYearlyDeforestationState

# Create a scatter plot of deforestation overtime for entire dataset
plot(dfYearlyDeforestation$Year, dfYearlyDeforestation$AverageDeforestation,
     main="Deforestation from 1999-2019",
     xlab="Years",
     ylab="Deforestation (km^2)",
     col="blue",
     type="l",
     xaxp = c(1999, 2019, 20),
     cex.axis=0.5)

# Create a bar plot of deforestation by state
barplot(dfStatesDeforestation$AverageDeforestation,names.arg=dfStatesDeforestation$State,
        main="Deforestation by State",
        ylab="Deforestation (km^2)",
        col=rainbow(20),
        las=2,
        cex.names=0.5)


# Find highest deforestation
maxDeforestation = max(dfDeforestationAll$Deforestation)
maxDeforestation

# Find the index with highest deforestation
index2 = as.numeric(rownames(dfDeforestationAll)[which.max(dfDeforestationAll$Deforestation)])
index2

# Find the year and state with highest deforestation
paste("The highest deforestation occurred in", dfDeforestationAll$State[index2], "in the year", dfDeforestationAll$Year[index2])

# Order data frame by highest deforestation in descending order
dfDeforestationAll[order(-dfDeforestationAll$Deforestation),]

# Correlation plot - Deforestation vs. Firespots
corrFD <- ggplot(yearlySummaryRefined, aes(x = Firespots, y = Deforestation)) + geom_point() + stat_smooth(method="lm")
corrFD <- corrFD + ggtitle("Regression Model - Deforestation vs. Firespots") + theme(plot.title = element_text(size = 9, hjust = 0.5)) 
corrFD

# ---------------------------- Description -------------------------------------

# The next series of functions and inline code creates two collections of maps.

# The first collection of maps is a series of heat maps which show significant 
# temperature change between the years 1999 and 2019, using the mean slope of 
# change to indicate the severity of the rise in temperature.

# The second collection of maps shows firespots around the stations that
# reported them. This colleciton focuses on 4 states, AMAZONAS, MATO GROSSO,
# PARA, and TOCANTINS, some of which experienced significant loss of forest
# due to man-made firespots. and some of which did not.

# -------------------------- End Description -----------------------------------

# register the google key so we can use ggmap
register_google(key="AIzaSyAPDi4PuY9YRPMjivKmTXQnaIi7XvwbB3A")

# from https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html
# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# uncomment to load states (takes awhile)
#region <- read_state()

# local working directory (where all data files are found)
wd <- "D:\\Karl\\1 Syracuse Masters Program\\2021 - Spring\\IST 687 - ISchool\\Final Project\\DataSets\\"

# Function from IST687 class homework 7 to remove axis formats from the heatmaps
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank()
)

# purpose:  gets the names and abbreviations of the Brazillian states we are most interested in
# returns:  a data frame with the Brizillian states we are most interested in
getStateNames <- function()
{
  # create a dataframe to contain states and abbreviations (to merge below)
  stateNames <- c("AMAZONAS", "MATO GROSSO", "PARA", "TOCANTINS")
  stateAbb <- c("AM", "MT", "PA", "TO")
  states <- data.frame(stateNames, stateAbb)
  colnames(states) <- c("stateName", "state")
  
  return(states)
}

# purpose: loads all station codes with name, state, lat, lon, and altitude
# returns:  the stations
# lastMod:  3/17/2021
getStations <- function()
{
  df <- setwd(wd)
  df <- read.csv("WeatherStations.csv")
  # rename columns
  colnames(df) <- c("station", "name", "state", "lat", "lon", "altitude")
  
  return(df)
}

# purpose: loads station codes for the states we are studying
# returns:  the stations
# lastMod:  3/7/2021
mungeStations <- function(showAllStates = FALSE)
{
  df <- getStations()
  
  if(showAllStates == FALSE)
    {
      states <- getStateNames()
      
      df1 <- df[df$state==c("AM"),] # amazonas state
      df2 <- df[df$state==c("MT"),] # Mato Grosso state
      df3 <- df[df$state==c("PA"),] # para state
      df4 <- df[df$state==c("TO"),] # Tocantins state
      
      df <- rbind(df1, df2, df3, df4)
      
      df <- merge(df, states, by="state")
  }
  
  return(df)
}

# purpose: loads all firespots from 1999 - 2019
# returns:  the a data frame containing the fires data
# lastMod:  3/10/2021
getFirespots <- function()
{
  # get firespots
  fires <- setwd(wd)
  fires <- read.csv("inpe_brazilian_amazon_fires_1999_2019.csv")
  colnames(fires)[3] <- c("stateName")
  stateNames <- getStateNames()
  fires <- merge(fires, stateNames, by="stateName")
  colnames(fires) <- c("stateName", "year", "month", "lat", "lon", "firespots", "state")
  
  return(fires)
}

# purpose: loads all significance results for temps from 1999-2019 
# returns:  returns sig data as well as lat, long, and state for stations
# lastMod:  3/17/2021
getTempSignificance <- function()
{
  # get the file
  temps <- setwd(wd)
  temps <- read.xlsx("Significance_results.xlsx")
  
  # rename columns so they match the stations table
  colnames(temps) <- tolower(colnames(temps))

  # merge with the stations table
  temps <- merge(temps, getStations(), by="station")

  return(temps)
}

# Purpose: gets monthly summary of temperatures
# returns: what you'd expect. ;)
getTemperatures <- function()
{
  df <- setwd(wd)
  df <- read.csv("monthlySummary.csv")
  
  return(df)
}

# purpose:    draws a topographical map of South America
# returns:    the topographical map
# parameters
#         zoomFactor: how close in we want to be. smaller numbers are farther away
#         centerLongitude: the mid point of longitude for our map
#         centerLatitude: the mid point of latitude for our map
# lastMod:    3/10/2021
draw.southAmerica <- function(topTitle="", topSubTitle="", color_Or_bW="bw", zoomFactor, centerLongitude=-55.509545, centerLatitude=-11.860846)
{
  # create the topographical map
  thisMap <- ggmap(get_googlemap(center = c(lon = centerLongitude, lat = centerLatitude), zoom = zoomFactor, scale = 2, maptype='terrain', color=color_Or_bW))
  
  # get rid of the axis
  thisMap <- thisMap
  
  # add a label
  thisMap <- thisMap + labs(title=topTitle, size=16) +
    labs(subtitle=topSubTitle, size=8) +
    xlab("") +
    ylab("") + 
    ditch_the_axes

  # show the map
  return(thisMap)
}

# purpose:    adds firespots as dots of varying sizes, depending on the size of the observed fire
# returns:    the topographical map with an added geom_point layer
# parameters
#         saMap:    the map to be altered - a map of south america created with ggmap
#         fires:    the fires that occurred with lat, lon, and "firespots" data, which is the size of the fire
#         mapTitle: the title of the map
#         mapSubTitle: the subtitle of the map
#         dotColor: the color of the dots to be plotted
# lastMod:    3/10/2021
add.FireDotMap <- function(saMap, fires, mapTitle="", mapSubTitle="", dotColor="#B00000")
{
  saMap <- saMap + geom_point(data=fires, aes(x=lon, y=lat), color=dotColor, size=log(fires$firespots))
  saMap <- saMap + ggtitle(mapTitle) +
    xlab("") +
    ylab("") + 
    labs(subtitle = mapSubTitle)
    theme(plot.title=element_text(hjust=0.5))
  
  return(saMap)
}

# Purpose:      Get a blank map of South America (non-topographical)
# parameters
#       region:     the data with lat and long
#       backColor: the background color for the map
# adapted from: https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
draw.blank.southAmerica <- function(region=NULL, backColor="#000000", outlineColor = "#FFFFFF")
{ 
  if(is.null(region) | length(region$code_state) == 0){
    region <- read_state() # function from the geobr library
  }
  
  #thisLat <- geocode("Ilha do Bananal, Tocantins", key=MyKey)[2] + 10
  
  return(
    ggplot() +
      geom_sf(data=region, fill=backColor, color=outlineColor, size=.15, show.legend = FALSE) +
      labs(subtitle="States", size=8) +
      xlab("") +
      ylab("") +
      coord_sf(ylim = c(-30, 3))
  ) 
}

# purpose:  create data frame with all state abbreviations and the x/y/locations where
#           they should appear on a map
# returns:  a data frame of state abbreviations with x and y coords where they should go on the map
# usage:    tagCoords <- stateTags
#           myMap <- myMap + annotate(geom="text", x=tagCoords$x, y=tagCoords$y, label = tagCoords$state, size = 4)
stateTags <- function()
{
  xCoords <- c(-64, -61.5, -51.5, -61.5, -53, -45, -42, -39.5, -36.5, -36.5, -38, -36.5, -37.5, -42, -48, -55, -55, -50, -47.5, -44, -55, -49, -51, -50.5, -53)
  yCoords <- c(-4, 2, 2, -12, -4, -4, -7, -4.5, -5.75, -7.25, -8.5, -9.5, -10.5, -12, -11, -13, -13, -17, -15.7, -19, -20, -21.5, -25, -27, -29.5)
  stateTags <- c('AM', 'RR', 'AP', 'RO', 'PA', 'MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA', 'TO', 'MT', 'MT', 'GO', 'DE', 'MG', 'MS', 'SP', 'PR', 'SC', 'RS')
  
  stateCoordinates <- data.frame(stateTags, yCoords, xCoords) 
  colnames(stateCoordinates) <- c("state","y","x")
  
  return(stateCoordinates) 
}

# Purpose:      Get a heat map of South America (non-topographical)
# returns:      heat map of brazil where state color is shown by slope of change
#               for states where significant change occurred during the given season
# parameters
#       sa:     the data with multi-polygons of states
#       season: the season in which the changes occurred
# adapted from: https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
draw.southAmerica.heatmap <- function(sa, season="Yearly", lowColor="#FFFFFF", highColor="Red")
{ 
  # ----- vars for inline run ----- #
  #season<-"Winter"
  #lowColor <- "#FFFFFF"
  #highColor <- "Red"
  
  # ----- end vars for inline run ----- #
  
  # shape files for all states in Brazil
  stateShapes <- sa[,c(2, 3, 6)]
  colnames( stateShapes)[1] <- "state" # make sure state col is named to match our data
  
  # get stations/seasons with significant temp change
  significantStations <- getTempSignificance()
  significantStations <- significantStations[,c(1, 2, 3, 8)][significantStations$significant=="Reject",]
  
  # if we selected a specific season, remove the remaining seasons
  if(tolower(season) %in% c("winter", "spring", "summer", "fall")){
    significantStations <- significantStations[significantStations$season == season,]
  }
  
  # get station codes by state
  stations <- getStations()[,c(1,3)]
  
  # merge stations with temp change to add state to stations
  significantStations <- merge(significantStations, stations, by="station")
  significantStations <- significantStations[order(significantStations$state),] # sort by state

  # get mean slope by state - this turns significantStations into a vector
  significantStations <- tapply(significantStations$slope, significantStations$state, mean)
  # turn it back into a data frame with 2 columns
  significantStations <- data.frame(rownames(significantStations), significantStations)
  colnames(significantStations) <- c("state", "meanSlope") # reset the col names
  
  # merge the stateShapes with significant stations
  mapData <- merge(stateShapes, significantStations, by="state")
  mapData <- mapData[,c(1,3,4)]
  
  # create df containing states with no change (to add to the mapData)
  statesWithNoChange <- merge(stateShapes, getStations(), by="state")
  # eliminate extra columns (even though geom column isn't mentioned, it will be included as "geometry")
  statesWithNoChange <- statesWithNoChange[,1]
  # RO doesn't have a station, but needs to be on the map
  fakeROStation <- getStations()[1,]
  fakeROStation[,3] <- "RO"
  RO <- merge(stateShapes[stateShapes$state == "RO", c(1, 3)], fakeROStation, by="state")
  # have to merge RO with something so that geom column will be renamed "geometry"
  # add RO to statesWithNoChange
  statesWithNoChange <- rbind(statesWithNoChange, RO[1])
  # add zero for all mean slopes in statesWIthNoChange
  statesWithNoChange$meanSlope <- rep(0,times=length(statesWithNoChange$state))
  # remove unnecessary columns
  statesWithNoChange <- statesWithNoChange[,c(1,3,2)]
  
  # remove states with change
  for(i in mapData$state)
  {
    statesWithNoChange <- statesWithNoChange[statesWithNoChange$state != i,]
  }

  lowColor <- "#FFFFFF"
  highColor <- "Red"
    
  # add states that had no change
  mapData <- rbind(mapData, statesWithNoChange)
  
  # create the
  returnMap <- ggplot(mapData, aes(fill=meanSlope, color=meanSlope)) + 
    labs(title=paste("Brazillian States with Significant Change in", season,"Temperature") , size=16) +
    labs(subtitle="Slope of Change from 1999 to 2019", size=8) +
    xlab("") +
    ylab("")
  returnMap <- returnMap + geom_sf()
  returnMap <- returnMap + coord_sf()
  # add color scale
  returnMap <- returnMap + scale_colour_gradient(
    low = lowColor,
    high = highColor
  )
  # add color gradient
  returnMap <- returnMap + scale_fill_gradient(
    low = lowColor,
    high = highColor
  )
  
  # add state labels
  tags <- stateTags()
  returnMap <- returnMap + annotate(geom="text", x=tags$x, y=tags$y, label = tags$state, size = 4)
  
  return(returnMap) 
}

#---------------add temp significance to map -----------------------
sa <- read_state() # function from the geobr library

# draw yearly heat map map
draw.southAmerica.heatmap(sa)

# draw heat maps by season
draw.southAmerica.heatmap(sa, "Winter")
draw.southAmerica.heatmap(sa, "Spring")
draw.southAmerica.heatmap(sa, "Summer")
draw.southAmerica.heatmap(sa, "Fall")

# draw the empty map of south america
draw.blank.southAmerica(sa, backColor = "#FFFFFF")

# example of how to read one state's data if we need in future
# paraState <- read_state(code_state="PA", year="2018")

#--------------- end add temp changes to map -----------------------

#--------------- map the firespots ---------------------------------

# store stations in variable and translate headers
stations <- mungeStations()
allStations <- mungeStations(TRUE)

# load all firespots from 1999 to 2019
fireSpots <- getFirespots()

# get firespots in 3-year buckets to output in separate maps below
fireSpots1 <- fireSpots[fireSpots$year  %in% c(1999, 2000, 2001),]
fireSpots2 <- fireSpots[fireSpots$year  %in% c(2002, 2003, 2004),]
fireSpots3 <- fireSpots[fireSpots$year  %in% c(2005, 2006, 2007),]
fireSpots4 <- fireSpots[fireSpots$year  %in% c(2008, 2009, 2010),]
fireSpots5 <- fireSpots[fireSpots$year  %in% c(2011, 2012, 2013),]
fireSpots6 <- fireSpots[fireSpots$year  %in% c(2014, 2015, 2016),]
fireSpots7 <- fireSpots[fireSpots$year  %in% c(2017, 2018, 2019),]

# central spot on map (several options)
#centerOfMap <- geocode("Alta Floresta, Mato Grosso", key=MyKey)
#centerOfMap <- geocode("Ilha do Bananal, Tocantins", key=MyKey)
centerOfMap <- geocode("Sinop, Mato Grosso", key=MyKey)
centerLat <- centerOfMap$lat
centerLon <- centerOfMap$lon

# zoom factor goes from 1 - 15 with 1 being the world and 15 being extremely close
zoomFact = 4

reds <- c("#FFFFFF","#FFEE00", "#FBB806", "#F6830C", "#F24D11", "#ED1717", "#550055")

# ______________________________________________________________________________
# NOTE: the compiler appears to get overwhelmed if you try to render all of your
#       plots at once. To avoid this, step through the lines below.
# ______________________________________________________________________________

# create the topographical map of South America
thisMap <- draw.southAmerica(topTitle="Brazil Firespots (Amazonas, Mato Grosso, Para, Tocantins)", color="bw", topSubTitle="", zoomFactor=zoomFact, centerLon, centerLat)
thisMap

# add firespots to the map, putting each map to the screen with each addition
thisMap <- add.FireDotMap(thisMap, fireSpots1, mapTitle="Brazil Firespots (Amazonas, Mato Grosso, Para, Tocantins)", mapSubTitle = "1999 - 2001", reds[1])
thisMap
thisMap <- add.FireDotMap(thisMap, fireSpots2, mapTitle="Brazil Firespots (Amazonas, Mato Grosso, Para, Tocantins)", mapSubTitle = "1999 - 2004", reds[2])
thisMap
thisMap <- add.FireDotMap(thisMap, fireSpots3, mapTitle="Brazil Firespots (Amazonas, Mato Grosso, Para, Tocantins)", mapSubTitle = "1999 - 2007", reds[3])
thisMap
thisMap <- add.FireDotMap(thisMap, fireSpots4, mapTitle="Brazil Firespots (Amazonas, Mato Grosso, Para, Tocantins)", mapSubTitle = "1999 - 2010", reds[4])
thisMap
thisMap <- add.FireDotMap(thisMap, fireSpots5, mapTitle="Brazil Firespots (Amazonas, Mato Grosso, Para, Tocantins)", mapSubTitle = "1999 - 2013", reds[5])
thisMap
thisMap <- add.FireDotMap(thisMap, fireSpots6, mapTitle="Brazil Firespots (Amazonas, Mato Grosso, Para, Tocantins)", mapSubTitle = "1999 - 2016", reds[6])
thisMap
thisMap <- add.FireDotMap(thisMap, fireSpots7, mapTitle="Brazil Firespots (Amazonas, Mato Grosso, Para, Tocantins)", mapSubTitle = "1999 - 2019", reds[7])
thisMap

#--------------- end map the firespots ---------------------------------

#--------------- begin plotting monthly temperatures for AM and PA -----

# get monthly summary of temperatures
temps <- getTemperatures()

# here we are constructing a Date column from Months and Years
months <- temps$Month
years <- temps$Year
days <- rep.int(1, length(temps$Year))
df1 <- data.frame(months, days, years)
temps$Date <- as.Date(paste(df1$year, '01', df1$month, sep='-'))

# use SQL different method to grab just PA and AM from the temps df
temps2 <- sqldf("SELECT * FROM temps WHERE state=='AM' OR state='PA' ORDER BY date")

# focus on most important columns
temps2 <- temps2[,c(2, 3, 4, 9, 17)]

AM <- temps2[temps2$State=="AM",]
PA <- temps2[temps2$State=="PA",]

# plot AM against PA to see 20-year trend for 2 states
ggplot(temps2, aes(x=Date, y=Avg_Temperature, color=State)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Average Monthly Temperature (°C), Year by Year, for 2 Parallel States",
       subtitle = "Amazonas(AM) vs Pará 1999 - 2019") 

#--------------- end plotting monthly temperatures for AM and PA -----




#--------------------deforestation maps--------------------------------
#Adds API key
register_google(key = "AIzaSyAV2RlEAaIOziz4LNzdco2DsXMHUKhZERk", write = TRUE)

#Create Map -----------------------------------------------------------------
#Colors
l1 <- "#ffee00"
l2 <- "#fbb806"
l3 <- "#f6830c"
l4 <- "#f24d11"
l5 <- "#550055"

#save values of center of map
center <- c(lon = -55.509545, lat = -11.860846)

#Function To remove axis (courtesy of professor Awaysheh)
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

#create base map
basemap <- get_map(source = "google", maptype ="roadmap", location = center, zoom = 4, color='bw')
basemap <- ggmap(basemap) +
  coord_fixed() +
  ditch_the_axes


# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# Next the shapefile has to be converted to a dataframe for use in ggplot2
# extension as arguments
shapefile2019 <- readOGR("Shapefiles/2019", "yearly_deforestation")
deforestation2019shp <- fortify(shapefile2019)
rm(shapefile2019)

shapefile2007 <- readOGR("Shapefiles/2007", "PDigital2007_AMZ_pol")
deforestation2007shp <- fortify(shapefile2007)
rm(shapefile2007)

shapefile2006 <- readOGR("Shapefiles/2006", "PDigital2006_AMZ_pol")
deforestation2006shp <- fortify(shapefile2006)
rm(shapefile2006)

shapefile2005 <- readOGR("Shapefiles/2005", "PDigital2005_AMZ_pol")
deforestation2005shp <- fortify(shapefile2005)
rm(shapefile2005)

shapefile2017to2019 <- readOGR("Shapefiles/2017-2019", "yearly_deforestation")
deforestation2017to2019shp <- fortify(shapefile2017to2019)
rm(shapefile2017to2019)

shapefile2014to2016 <- readOGR("Shapefiles/2014-2016", "yearly_deforestation")
deforestation2014to2016shp <- fortify(shapefile2014to2016)
rm(shapefile2014to2016)

shapefile2011to2013 <- readOGR("Shapefiles/2011-2013", "yearly_deforestation")
deforestation2011to2013shp <- fortify(shapefile2011to2013)
rm(shapefile2011to2013)

shapefile2008to2010 <- readOGR("Shapefiles/2008-2010", "yearly_deforestation")
deforestation2008to2010shp <- fortify(shapefile2008to2010)
rm(shapefile2008to2010)





#Creates geoms by year
geom2017to2019 <- geom_polygon(data = deforestation2017to2019shp, aes(x = long, y = lat, group = group), fill = l5, colour = l5)
geom2014to2016 <- geom_polygon(data = deforestation2014to2016shp, aes(x = long, y = lat, group = group), fill = l4,  colour = l4)
geom2011to2013 <- geom_polygon(data = deforestation2011to2013shp, aes(x = long, y = lat, group = group), fill = l3, colour = l3)
geom2008to2010 <- geom_polygon(data = deforestation2008to2010shp, aes(x = long, y = lat, group = group), fill = l2, colour = l2)

geom2019 <- geom_polygon(data = deforestation2019shp, aes(x = long, y = lat, group = group), fill = l5, colour = l5)
geom2007 <- geom_polygon(data = deforestation2007shp, aes(x = long, y = lat, group = group), fill = l1, colour = l1)
geom2006 <- geom_polygon(data = deforestation2006shp, aes(x = long, y = lat, group = group), fill = l1, colour = l1)
geom2005 <- geom_polygon(data = deforestation2005shp, aes(x = long, y = lat, group = group), fill = l1, colour = l1)



#Create one map for 2017 through 2019
deforestation2017to2019 <- ggmap(basemap) + geom2017to2019
#Create one map for 2014 through 2016
deforestation2014to2016 <- ggmap(basemap) + geom2014to2016
#Create one map for 2011 through 2013
deforestation2011to2013 <- ggmap(basemap) + geom2011to2013
#Create one map for 2008 through 2010
deforestation2008to2010 <- ggmap(basemap) + geom2008to2010
#Create one map for 2005 through 2007
deforestation2005to2007 <- ggmap(basemap) + geom2005 + geom2006 + geom2007

#Creates map to compare 2005 and 2019
deforestation2005to2007 <- ggmap(basemap) + geom2005 + geom2019


#Creates map to compare all
deforestationl5 <- basemap + geom2017to2019 + geom2014to2016 + geom2011to2013 + geom2008to2010 + geom2005 + geom2006 + geom2007

#Creates map to compare all
deforestationl4 <- basemap + geom2014to2016 + geom2011to2013 + geom2008to2010 + geom2005 + geom2006 + geom2007

#Creates map to compare all
deforestationl3 <- basemap + geom2011to2013 + geom2008to2010 + geom2005 + geom2006 + geom2007

#Creates map to compare all
deforestationl2 <- basemap + geom2008to2010 + geom2005 + geom2006 + geom2007

#Creates map to compare all
deforestationl1 <- basemap + geom2005 + geom2006 + geom2007

#-------------------------------Temperature Significance Testing-------------------------#
#-------------------------------Summarized by station, year, season----------------------#

#create seasons
seasons <- data.frame(cbind(c("Spring","Spring","Spring",
                              "Summer","Summer","Summer",
                              "Fall","Fall","Fall",
                              "Winter","Winter","Winter"),
                            c("October", "November","December",
                              "January", "February", "March",
                              "April","May","June",
                              "July","August","September")))

#combine seasons with weather data
colnames(seasons)<-c("Season","Month")
BrazilWeather$Month <- months(BrazilWeather$Date)

df_weather <- merge(x = BrazilWeather, y = seasons, by = "Month", all.x = TRUE)
df_weather$Year <- year(df_weather$Date)

#summarize weather data
min_temp <- df_weather[,c(2,12,9,13)]
min_summ <- min_temp %>% group_by(Station,Year,Season) %>% summarise_all(list(     
  mean_min = ~ mean(., na.rm = TRUE),
  median_min = ~ median(.,na.rm= TRUE),
  s_dev_min = ~ sd(., na.rm = TRUE),
  sum_min = ~ sum(.,na.rm = TRUE))) 

avg_temp <- df_weather[,c(2,12,8,13)]
avg_summ <- avg_temp %>% group_by(Station,Year,Season) %>% summarise_all(list(     
  mean_av = ~ mean(., na.rm = TRUE),
  median_av = ~ median(.,na.rm= TRUE),
  s_dev_av = ~ sd(., na.rm = TRUE),
  sum_av = ~ sum(.,na.rm = TRUE))) 

max_temp <- df_weather[,c(2,12,7,13)]
max_summ <- max_temp %>% group_by(Station,Year,Season) %>% summarise_all(list(     
  mean_max = ~ mean(., na.rm = TRUE),
  median_max = ~ median(.,na.rm= TRUE),
  s_dev_max = ~ sd(., na.rm = TRUE),
  sum_max = ~ sum(.,na.rm = TRUE))) 

#re-merge weather data to get all temperature aggregations
temp_by_stationSeason <- merge(merge(min_summ,avg_summ,by = c("Season","Year","Station")),max_summ,by =  c("Season","Year","Station"))

#check for distributions quickly
temp_sub <- df_weather[df_weather$Station=="82024"& df_weather$Season=="Winter",]
hist(temp_sub$`Avg_Temperature_(Â°C)`)

#preparing the dataset for linear reg
temp_by_stationSeason <- temp_by_stationSeason[temp_by_stationSeason$mean_av!="NaN",]
temp_by_stationSeason$Year <- temp_by_stationSeason$Year-1998
temp_by_stationSeason$n_av <- temp_by_stationSeason$sum_av/temp_by_stationSeason$mean_av
temp_by_stationSeason <- temp_by_stationSeason[temp_by_stationSeason$n_av>74,]

#Begin forloop to obtain trend line & std deviation
#init variables
stn <- c()
seas <- c()
slope <- c()
std_err <- c()
degf <- c()

#looping over st = stations, ss = seasons (inner)
for(st in unique(temp_by_stationSeason$Station)){
  
  temp_stn <- temp_by_stationSeason[temp_by_stationSeason$Station==st,c(1,2,3,8,9,10)]
  
  for(ss in c("Spring","Summer","Fall","Winter")){
    
    temp_seas <- temp_stn[temp_stn$Season==ss,]
    
    if(nrow(temp_seas)<2){next}
    
    stn<-append(stn,st)
    seas <- append(seas,ss)
    
    #begin model
    mod <- lm(mean_av ~ Year, data = temp_seas)
    
    #getting model parameters/results
    std_err <- append(std_err,coef(summary(mod))[2,2])
    slope <- append(slope,coef(summary(mod))[2,1])
    degf <- append(degf, nrow(temp_seas)-1)
  }
}

#combine vectors to create new dataframe
df_lin <- data.frame(cbind(stn,seas,as.numeric(slope),as.numeric(std_err),degf))
colnames(df_lin) <- c("Station","Season","slope","std_err","degf")
df_lin$degf <- as.numeric(df_lin$degf)

#more cleaning with degf>19 (need all years present)
df_lin <- df_lin[df_lin$degf>19,]
df_lin$t_stat <- as.numeric(df_lin$slope)/as.numeric(df_lin$std_err)

#get p-values
df_lin$p_val <- pt(as.numeric(df_lin$t_stat),as.numeric(df_lin$degf),lower.tail = FALSE)

#ifelse to get reject or not reject
df_lin$significant <- ifelse(df_lin$p_val < 0.05,"Reject", "Fail to Reject")