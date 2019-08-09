
##Istnalling Packages used in the code below
#install.packages("leaflet")
#install.packages("rgdal")
#install.packages("plyr")
#install.packages("reshape2")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("rmapshaper")
#install.packages("leaflet.extras")




##Loading the libraries
library(rgdal)
library(plyr)
library(reshape2)
library(ggplot2)
library(rmapshaper)
library(leaflet.extras)



##Loading the data
waste_data = read.csv("clean_data.csv",header = TRUE)
#waste_data

# Fix for Mornington Peninsula
waste_data$Local.Government = revalue(waste_data$Local.Government, c("PENINSULA" = 'MORNINGTON PENINSULA'))

##removing commas from numbers and then converting it to numeric form
waste_data$X2018 <- as.numeric(as.character(gsub(",","",waste_data$X2018)))
waste_data$X2035 <- as.numeric(as.character(gsub(",","",waste_data$X2035)))



##adding the 2018 and 2035 waste generated per Council
waste_sum = setNames(aggregate(list(waste_data$X2018,waste_data$X2035), by=list(Category=waste_data$Local.Government), FUN=sum),c('Name',2018,2035))
waste_sum$Name = trimws(waste_sum$Name)
waste_sum = waste_sum[order(waste_sum$Name),]

##counting collectin points per council
count_waste = setNames(as.data.frame(table(waste_data$Local.Government)),c('Name','Count'))
#count_waste

##loading shapefile
spdf = readOGR(dsn = getwd(), layer = "VIC_LGA_POLYGON_shp")


# Code that is used to make deployment version more lightweight.
##This .RData file will be used in app.R file to load data
spdf = rmapshaper::ms_simplify(spdf, keep = 0.05)
save.image("mapping_data.RData")
