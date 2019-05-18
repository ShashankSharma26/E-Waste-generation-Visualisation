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


#setwd("/Users/shashanksharma/Desktop/iter 1 final")
##libraries used
library(leaflet)
library(rgdal)
library(plyr)
library(reshape2)
library(ggplot2)
library(rmapshaper)
library(leaflet.extras)
library(tippy)
library(bigrquery)


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
# spdf = rmapshaper::ms_simplify(spdf, keep = 0.05)
# save.image("mapping_data.RData")
# load("mapping_data.RData")


## Datafile for merging LGA data and contact information.
## Downloaded from Google Bigquery
# Bigquery service token
set_service_token('e-waste-database-2d9d97da8b37.json')

tb <- bq_dataset_query(
  x = "e-waste-database.db_council",
  query = "SELECT * FROM lga_details"
)

citycode <- bq_table_download(tb)

citycode$Name = trimws(citycode$Name) # Trim ws
names(citycode)[names(citycode) == "Code"] <- "LG_PLY_PID"



#head(citycode)

##merging all the files to the shapdefile data
spdf@data = data.frame(spdf@data, citycode[match(spdf@data$LG_PLY_PID, citycode$LG_PLY_PID),])
spdf@data = data.frame(spdf@data, waste_sum[match(spdf@data$Name, waste_sum$Name),])
spdf@data = data.frame(spdf@data, count_waste[match(spdf@data$Name, count_waste$Name),])


#removing nas in the data
# spdf@data$`X2018`[is.na(spdf@data$`X2018`)] = 0
# spdf@data$`X2035`[is.na(spdf@data$`X2035`)] = 0
spdf@data$Count[is.na(spdf@data$Count)] = 0


#unique(spdf@data$Count)

#melting for grouped bar chart
boxplot_data <- melt(waste_sum, id=c("Name"))


##colouring the map based on number of collection points in region
pal <- colorFactor(
  palette = c('darkgreen','lightgreen','yellow','orange','darkred'),
  #  domain = spdf@data$Count
  domain = spdf@data$`X2018`
)


ui <- fluidPage(tags$style(HTML("
                                .tabs-above > .nav > li[class=active] > a {
                                background-color: #000;
                                color: #FFF;
                                }")),
  tabsetPanel(
    tabPanel(
      h6(strong("All Regions"),style = "color:blue;font-family: 'georgia';"), value = 1,##for all regions 
      
      
      mainPanel(
        conditionalPanel(
          condition = "(input.tabs == 1)",
          h3("E-waste generation in Victoria", align = "center", style = "color:blue;font-family: 'georgia';"),
          
          leafletOutput("mymap",width = 1000, height = 600)
        )
      )
    )
    
    ,
    
    tabPanel(
      h6(strong("Compare Councils"),style = "color:blue;font-family: 'georgia';"), value = 2,##for comparison
      sidebarLayout(
        sidebarPanel(
          selectInput("first",                                                 ##selection input
                      h6(strong("Select first council"),style = "color:brown;font-family: 'georgia';"),
                      choices = unique(boxplot_data$Name)
          ),
          selectInput("second",                                                 ##selection input
                      h6(strong("Select second council"),style = "color:brown;font-family: 'georgia';"),
                      choices = unique(boxplot_data$Name),
                      selected = c("ARARAT")
                      
          )
        )
        ,
        mainPanel(
          plotOutput("comparison") 
        )
      ) 
    ),
    id= 'tabs'
)
)






server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({          ##leaflet output
    ##leaflet visualising the data
    ##leaflet visualising the data
    leaflet(spdf) %>% 
      #        addProviderTiles('CartoDB.PositronNoLabels') %>%
      addTiles() %>% 
      setView(lat=-36.6, lng=145.5 , zoom=6.75) %>% 
      addPolygons(fillColor = ~pal(`X2018`),
                  fillOpacity = 0.5,
                  weight = 1,
                  highlight = highlightOptions(weight = 2,
                                               color = "red",
                                               fillOpacity = 0.70,
                                               bringToFront = TRUE),
                  popup= paste(strong("Council name:"),spdf@data$Name,"<br><br>",
                               strong("E-waste generated in 2018:"),spdf@data$`X2018`," tonnes<br>",
                               strong("E-waste estimated in 2035:"),spdf@data$`X2035`," tonnes<br><br>",
                               strong("Council E-mail:"),spdf@data$Email,"<br>",
                               strong("Council website:"),paste0( "<a href='http://", spdf@data$Website, "' target='_blank'>", "Click Here</a>")
                               
                  )
                  ,
                  label=~Name,
                  group = spdf@data$Name)%>%
      
      
      #addTooltip(session, id, title, placement = "bottom", trigger = "hover", options = NULL)
      
      
      # addControl(
      #   "<font><B>Search Councils below</B></font>",
      #   position='topleft' ) %>%
      
      ##for searching purposes
      addSearchFeatures( 
        
        targetGroups  = spdf@data$Name,
        options = searchFeaturesOptions(position = "topleft",zoom=9.5, openPopup=TRUE,autoCollapse = FALSE, collapsed=FALSE, textPlaceholder = "Search councils")) %>%
      
        addLegend("topright", 
                colors =c('darkred','orange','yellow','lightgreen','darkgreen'),
                labels= c('5000 - 3000','3000-1000','1000-500','500-100','100-0'),
                title= "Tonnes of E-waste generated in 2018",
                opacity = 1) %>%

      # addControl(
      #   "<P><center><B><font color='red'>Hint!</font></B></center>Search for council names<br/>Example:<br/><ul><li>Monash</li><li>Hume</li><li>Melbourne</li><li>Knox</li></ul></P>",
      #   position='bottomright')%>%
      addControl(
        "<font><B>Reset Map Below</B></font>",
        position='topleft')%>%
      addResetMapButton()
      

  })
  
  
  
  output$comparison <- renderPlot(     ##boxplot output
    {
      first_selection <- as.character(input$first)
      second_selection <- as.character(input$second)
      plot_data = boxplot_data[which ((boxplot_data$Name == first_selection) | (boxplot_data$Name == second_selection)), ]
      
      ggplot(plot_data, aes(factor(plot_data$variable), plot_data$value, fill =plot_data$Name)) +
        geom_bar(stat="identity", position = "dodge") +  ggtitle("Comparison of E-waste generated") +
        scale_fill_brewer(palette = "Set1") +xlab("Year")+ylab("Tonnes of E-waste generated") + labs(fill = "Council") +
        theme(axis.text=element_text(size=18), axis.title=element_text(size=22), plot.title = element_text(size=28)
              ,legend.text = element_text(size=16), legend.title = element_text(size=22))
      
    }, height = 700, width = 700)
  
}

shinyApp(ui, server)


