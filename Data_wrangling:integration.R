#install.packages("leaflet")
#install.packages("rgdal")
#install.packages("plyr")
#install.packages("reshape2")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("rmapshaper")
#install.packages("leaflet.extras")
#install.packages("tippy")
#install.packages("bigrquery")
#install.packages("shinyWidgets")



##libraries used
library(rgdal)
library(plyr)
library(reshape2)
library(ggplot2)
library(rmapshaper)
library(leaflet.extras)
# library(tippy)
#library(bigrquery)
#library(shinyWidgets)


##data from the venar list fole named clean_data
waste_data = read.csv("clean_data.csv",header = TRUE)
#waste_data

# Fix for Mornington Peninsula
waste_data$Local.Government = revalue(waste_data$Local.Government, c("PENINSULA" = 'MORNINGTON PENINSULA'))

##removing commas from numbers and then converting it ti numeric form
waste_data$X2018 <- as.numeric(as.character(gsub(",","",waste_data$X2018)))
waste_data$X2035 <- as.numeric(as.character(gsub(",","",waste_data$X2035)))



##adding the 2018 and 2035 waste generated per LG
waste_sum = setNames(aggregate(list(waste_data$X2018,waste_data$X2035), by=list(Category=waste_data$Local.Government), FUN=sum),c('Name',2018,2035))
waste_sum$Name = trimws(waste_sum$Name)

waste_sum = waste_sum[order(waste_sum$Name),]

##counting collectin points per lg
count_waste = setNames(as.data.frame(table(waste_data$Local.Government)),c('Name','Count'))
#count_waste

##loading shapefile
spdf = readOGR(dsn = getwd(), layer = "VIC_LGA_POLYGON_shp")


# Code that is used to make deployment version more lightweight.
spdf = rmapshaper::ms_simplify(spdf, keep = 0.05)
save.image("mapping_data.RData")