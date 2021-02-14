



setwd('C:/Users/hilda/VisualizationData/cs424-program01')
energy<-read.csv(file = 'annual_generation_state.csv')
energy$GENERATION..Megawatthours. <- gsub(',', '', energy$GENERATION..Megawatthours.)
energy$GENERATION..Megawatthours. <- as.numeric(energy$GENERATION..Megawatthours.)
energy[order(energy$STATE),]
energy[,2] = toupper(energy[,2])
energy <- subset(energy, energy$GENERATION..Megawatthours.>=0)
energy <- subset(energy, energy$STATE != "  ")

energy <- subset(energy, ENERGY.SOURCE !="Other")
energy <- subset(energy, ENERGY.SOURCE !="Other Gases")
energy <- subset(energy, ENERGY.SOURCE !="Other Biomass")
energy <- subset(energy, ENERGY.SOURCE !="Pumped Storage")

energy$STATE <- as.factor(energy$STATE)
energy$TYPE.OF.PRODUCER<- as.factor(energy$TYPE.OF.PRODUCER)
energy$ENERGY.SOURCE<- as.factor(energy$ENERGY.SOURCE)

levels(energy$ENERGY.SOURCE)[levels(energy$ENERGY.SOURCE) == "Hydroelectric Conventional"]  <- "Hydro"
levels(energy$ENERGY.SOURCE)[levels(energy$ENERGY.SOURCE) == "Wood and Wood Derived Fuels"]  <- "Wood"
levels(energy$ENERGY.SOURCE)[levels(energy$ENERGY.SOURCE) == "Solar Thermal and Photovoltaic"]  <- "Solar"


#sapply(energy, levels)    # displays the levels in the data frame
library(ggplot2) 
library(usmap)
library(dplyr)
theme_set(theme_minimal())

energy <- rename(energy, c(MegawattHours="GENERATION..Megawatthours."))
energy <- rename(energy, c(EnergySource="ENERGY.SOURCE"))
energy <- rename(energy, c(TypeOfProducer="TYPE.OF.PRODUCER"))
energy <- rename(energy, c(State="STATE"))
energy <- rename(energy, c(Year="YEAR"))

# Total energy information for the US - not specific to state
usTotalEnergySources <- subset(energy, State == "US-TOTAL")
# We get rid of Total as an ENERGY.SOURCE since they are summary values of type of producer by state
usTotalEnergySources <- subset(usTotalEnergySources, EnergySource != "Total")



# stacked bar chart showing the amount of each energy source per year from 1990 - 2019 in the US
USTotalEnergySourceAmountStackedBar <- ggplot(usTotalEnergySources, aes(fill=EnergySource, x=Year, y=MegawattHours, width=0.8)) + 
  geom_bar(stat="identity")+scale_fill_manual(values=c('#d8b365','#8e0152', '#67a9cf', '#a1d76a', '#af8dc3', '#f1a340', '#e9a3c9', '#5ab4ac', '#4575b4')) +
  ggtitle("Energy sources in the United States from 1990 to 2019") +
  labs(y="Total Megawatt Hours", x = "Year", fill = "Energy Sources")

#--------------------------------------------------------------------------------------------

# stacked bar chart showing percent of the total production for each energy source per year from 1990 - 2019
USTotalEnergySourcePercentageStackedBar <- ggplot(usTotalEnergySources, aes(fill=EnergySource,x=Year, y=MegawattHours, width=0.8)) + 
  geom_bar(position="fill", stat="identity")+scale_fill_manual(values=c('#d8b365','#8e0152', '#67a9cf', '#a1d76a', '#af8dc3', '#f1a340', '#e9a3c9', '#5ab4ac', '#4575b4')) +
  ggtitle("Energy sources in the United States from 1990 to 2019") +
        labs(y="Total Megawatt Hour Percentage", x = "Year", fill = "Energy Sources") 

USYearlyEnergySource <- aggregate(MegawattHours ~ EnergySource + Year, data=usTotalEnergySources, FUN=sum)

# line chart showing the amount of each energy source per year from 1990 - 2019
USTotalEnergySourceAmountLineChart <- ggplot(USYearlyEnergySource, aes(x=Year, y=MegawattHours, group=EnergySource, color=EnergySource, linetype=EnergySource)) + geom_line(size =1.5) + 
  scale_color_manual(values=c('#d8b365','#8e0152', '#67a9cf', '#a1d76a', '#af8dc3', '#f1a340', '#e9a3c9', '#5ab4ac', '#4575b4'))+  
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "twodash", "solid", "dotted", "dotted"))+
  ggtitle("Energy sources in the United States from 1990 to 2019") + labs(y="Total Megawatt Hours", x = "Year", fill = "Energy Sources")

library("data.table")

# table of raw numbers for the amount of each energy source per year from 1990 - 2019
USTotalEnergySourceAmountRawTable = USYearlyEnergySource
setDT(USTotalEnergySourceAmountRawTable)         
setcolorder(USTotalEnergySourceAmountRawTable, c(2,1,3))

EnergyYearlyTotals <- aggregate(MegawattHours ~ Year, data=USYearlyEnergySource, FUN=sum)
# How many replicates you want of each row
duptimes <- c(9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9)

# Create an index of the rows you want with duplications
idx <- rep(1:nrow(EnergyYearlyTotals), duptimes)

# Use that index to generate your new data frame
EnergyYearlyTotals <- EnergyYearlyTotals[idx,]

USYearlyEnergySource$Percentage <- (USYearlyEnergySource$MegawattHours/EnergyYearlyTotals$MegawattHours)*100

# line chart showing the percent of the total production for each energy source per year from 1990 - 2019
USTotalEnergySourcePercentageLineChart <- ggplot(USYearlyEnergySource, aes(x=Year, y=Percentage, group=EnergySource, color=EnergySource, linetype=EnergySource)) + geom_line(size =1.5) + 
  scale_color_manual(values=c('#d8b365','#8e0152', '#67a9cf', '#a1d76a', '#af8dc3', '#f1a340', '#e9a3c9', '#5ab4ac', '#4575b4'))+  
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "twodash", "solid", "dotted", "dotted"))+
  ggtitle("Energy sources in the United States from 1990 to 2019") + labs(y="Total Megawatt Hour Percentage (%)", x = "Year", fill = "Energy Sources")

# table of raw numbers for the percent of the total production for each energy source per year from 1990 - 2019
USTotalEnergySourcePercentRawTable = USYearlyEnergySource
setDT(USTotalEnergySourcePercentRawTable) 
USTotalEnergySourcePercentRawTable[,MegawattHours:=NULL]
setcolorder(USTotalEnergySourcePercentRawTable, c(2,1,3))




















