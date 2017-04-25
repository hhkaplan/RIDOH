#MAP PRODUCER
library('ggplot2')
library('akima')
library('maps')

dat <- read.csv("/Users/Hannah/Documents/RIDOH/RIDOH Supporting Files.csv",header=TRUE, stringsAsFactors=FALSE)
dat2014 <- read.csv("/Users/Hannah/Documents/RIDOH/Database_2014.csv", stringsAsFactors=F)
parameters <- c("1,1,1 Trichloroethane - 34506", "1,2 Dichlorobenzene - 34536", "1,4 Dichlorobenzene - 34571" , "Alkalinity - 00410"
, "Benzene - 34030" , "Calcium, Dissolved -00915" , "Chloride - 00940", "Chloroform - 32106", "Chlorophyll a, water, fluorometric method, corrected for pheophytin - 32209"
, "Depth to bottom from water surface, at sampling location, meters - 82903" , "Dissolved Oxygen - 00300" , "E.coli - 31633"
, "Enterococci - 31639", "Ethylbenzene - 34371" , "Fecal Coliform - 74055" , "Magnesium, Dissolved - 00925" , "Methylene Chloride - 34423"
, "Nitrate + Nitrite, Dissolved - 00631" , "Nitrogen, Ammonia Dissolved as N - 00608" , "Nitrogen, Total (unfiltered) - 00600"
, "pH - 00400" , "Phosphorus, Dissolved - 00666" , "Phosphorus, Total - 00665" , "Salinity, (ppt) - 00480" , "Secchi Depth - 00069"
, "Sodium, Dissolved as NA - 00930" , "Sulfate as SO4 - 00945" , "Temperature - 00011" , "Tetrachloroethylene - 34475" , "Toluene - 34010"
, "Trichloroethylene - 39130" , "TSS - 00530")

county_map = map_data("county", region = "rhode island")
state_map = map_data("state", region = "rhode island")

#Run through all the parameters and make a plot for each one
for (i in 1:length(parameters)) {
  
  #Sort through and find all the "i parameter" measurements:
  Index = which(dat2014$Parameter == parameters[i])
  redData = dat2014[Index,]
  
  #skip if there are no entries
  if (nrow(redData)==0) {
    print(paste("No data for", parameters[i], sep = ' '))
  }
  else {
    #Find the average "i parameter" at every site
    redData_mean = aggregate(redData$Concentration, list(Station.Name = redData$Station.Name), mean, na.rm=TRUE)
    
    #Get names, lats, and longs of all sites from the original dataset we used 
    latlong = data.frame(dat[,2], dat[,11], dat[,12])
    colnames(latlong) = c("Station.Name", "Latitude", "Longitude")
    
    #Add long and lat to the 2014 data by linking the two datasets by site name
    mergedData <- merge(redData_mean, latlong, by="Station.Name")
    
    #Make some plots and print them to new windows with the right titles
    
    pointplot <- ggplot()+ 
      geom_polygon(data=county_map, aes(x=long, y=lat, group=group)) +
      geom_polygon(data =state_map, aes(x=long, y=lat, group=group), col = "red") +
      geom_point(data=mergedData, aes(x=Longitude, y=Latitude, color = x)) +
      scale_color_gradient(low = "#0000FFFF", high = "#FF0000FF",guide = "colourbar") +
      ggtitle(parameters[i]) + coord_fixed(1.2)
    
   
    d <- na.omit(mergedData)
    int <-interp2xyz(interp(x=d$Longitude, y=d$Latitude, z=d$x, duplicate="mean", extrap=TRUE))
    int <- as.data.frame(int)
    int <- na.omit(int)
    
    interpplot<- ggplot() +
      geom_raster(data = int, aes(x = x, y = y, fill = z), interpolate = TRUE) +
      scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
      geom_contour(data = int, aes(x = x, y = y, z = z), color = "black", alpha = 0.1)+
      geom_polygon(data =state_map, aes(x=long, y=lat, group = group), fill = NA, col = "red") +
      ggtitle(parameters[i]) + coord_fixed(1.2)
   
    #create a new window and print onto it
    dev.new()
    print(pointplot)  
    dev.new()
    print(interpplot)
    }
}