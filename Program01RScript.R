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


sapply(energy, levels)    # displays the levels in the data frame

