library(ggplot2)

site273 <- read.csv("/Users/Hannah/Documents/RIDOH/WW273.csv", stringsAsFactors=F)
parameters <- c("1,1,1 Trichloroethane - 34506", "1,2 Dichlorobenzene - 34536", "1,4 Dichlorobenzene - 34571" , "Alkalinity - 00410"
                , "Benzene - 34030" , "Calcium, Dissolved -00915" , "Chloride - 00940", "Chloroform - 32106", "Chlorophyll a, water, fluorometric method, corrected for pheophytin - 32209"
                , "Depth to bottom from water surface, at sampling location, meters - 82903" , "Dissolved Oxygen - 00300" , "E.coli - 31633"
                , "Enterococci - 31639", "Ethylbenzene - 34371" , "Fecal Coliform - 74055" , "Magnesium, Dissolved - 00925" , "Methylene Chloride - 34423"
                , "Nitrate + Nitrite, Dissolved - 00631" , "Nitrogen, Ammonia Dissolved as N - 00608" , "Nitrogen, Total (unfiltered) - 00600"
                , "pH - 00400" , "Phosphorus, Dissolved - 00666" , "Phosphorus, Total - 00665" , "Salinity, (ppt) - 00480" , "Secchi Depth - 00069"
                , "Sodium, Dissolved as NA - 00930" , "Sulfate as SO4 - 00945" , "Temperature - 00011" , "Tetrachloroethylene - 34475" , "Toluene - 34010"
                , "Trichloroethylene - 39130" , "TSS - 00530")

#Run through all the parameters and make a plot for each one
for (i in 1:length(parameters)) {
  
  #Find parameter we want to look at 
  Index_273 = which(site273$Parameter == parameters[i])
  Data_273 = site273[Index_273,]
  
  #Print a warning if there is no data
  if (nrow(Data_273)==0) {
    print(paste("No data for", parameters[i], sep = ' '))
  }
  else {
  
    #Convert dates into a format R can read
    Data_273$Date <- as.Date(Data_273$Date, "%m/%d/%Y")
    
    #Plot a timeline
    plotit <- ggplot(data = Data_273, aes(Date, Concentration))+ geom_point()+
    ggtitle(parameters[i]) 
    
    #create a new window and print onto it
    dev.new()
    print(plotit)
  }
}