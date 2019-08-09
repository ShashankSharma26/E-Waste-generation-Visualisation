##Intall required packages
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



##Loading packages
library(leaflet)
# library(rgdal)
library(plyr)
library(reshape2)
library(ggplot2)
# library(rmapshaper)
library(leaflet.extras)
# library(tippy)
library(bigrquery)
library(shinyWidgets)

##loading the mapping data saved using wrangling and integraton file to load the app quickly
load("mapping_data.RData")


##Reading Council local code data
citycode <- read.csv("LOCAL_CODE.csv")
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
  domain = spdf@data$`X2018`
)

##########################################
######USER INTERFACE CODE#################
##########################################

ui <- fluidPage(tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #D3D3D3;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: grey; color:black}
     ")),
                
  setBackgroundColor("offwhite"),
  
  tabsetPanel(                                             ##setting tab values
    
    tabPanel(
      h6(strong("All Councils"),style = "color:black;font-family: 'arial';"), value = 1,##for all regions 
      mainPanel(
        conditionalPanel(
                      condition = "(input.tabs == 1)",
                      h3("E-waste generation in Victoria", align = "center", style = "color:black;font-family: 'arial';"),
                      leafletOutput("mymap",width = 1200, height = 700)
                      )
               )
          ),
    
    
    tabPanel(
      h6(strong("Compare Councils"),style = "color:black;font-family: 'arial';"), value = 2,##for comparison
      
      sidebarLayout(
          sidebarPanel(
            selectInput("first",                                                 ##selection input
                        h6(strong("Select first council"),style = "color:brown;font-family: 'arial';"),
                        choices = unique(boxplot_data$Name)
                      ),
            selectInput("second",                                                 ##selection input
                        h6(strong("Select second council"),style = "color:brown;font-family: 'arial';"),
                        choices = unique(boxplot_data$Name),
                        selected = c("ARARAT")
                      
                        )
                      ),
          
          mainPanel(
            plotOutput("comparison") 
                    )
          
            ) 
          ),
    
    id= 'tabs'
      )
)


##########################################
#############SERVER CODE#################
##########################################

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({          ##leaflet output

    ##leaflet visualising the data
    leaflet(spdf) %>% 
      addTiles() %>% 
      
      ###setting map view to victoria's lattitude and longitude#####
      setView(lat=-36.6, lng=145.5 , zoom=7) %>% 
      
      ###adding poylgons for councils shapes####
      addPolygons(fillColor = ~pal(`X2018`),
                  fillOpacity = 0.5,
                  weight = 1,
                  highlight = highlightOptions(weight = 2,
                                               color = "red",
                                               fillOpacity = 0.70,
                                               bringToFront = TRUE),
                  popup= paste(strong("Council name:"),spdf@data$Name,"<br><br>",                          ##for popup data
                               strong("E-waste generated in 2018:"),spdf@data$`X2018`," tonnes<br>",
                               strong("E-waste estimated in 2035:"),spdf@data$`X2035`," tonnes<br><br>"
                               ),
                  label=~Name,                    ##for tooltip information
                  group = spdf@data$Name
                  )%>%
     
      ### to search for coouncils in the map
      addSearchFeatures(                    
        targetGroups  = spdf@data$Name,
        options = searchFeaturesOptions(position = "topleft",
                                        zoom=9.5, 
                                        openPopup=TRUE,
                                        autoCollapse = FALSE, 
                                        collapsed=FALSE, 
                                        textPlaceholder = "Search councils")
                      ) %>%
      
      addLegend("topright", 
                colors =c('darkred','orange','yellow','lightgreen','darkgreen'),
                labels= c('5000 - 3000','3000-1000','1000-500','500-100','100-0'),
                title= "Tonnes of E-waste generated in 2018",
                opacity = 1) %>%

      addControl(
        "<font><B>Reset Map Below</B></font>",
        position='topleft')%>%
      addResetMapButton()           ##to reset to default view
      

  })
  
  
  
  output$comparison <- renderPlot(     ##boxplot output
    {
      first_selection <- as.character(input$first)
      second_selection <- as.character(input$second)
      plot_data = boxplot_data[which ((boxplot_data$Name == first_selection) | (boxplot_data$Name == second_selection)), ]
      
      ggplot(plot_data, aes(factor(plot_data$variable), plot_data$value, fill =plot_data$Name)) +
        geom_bar(stat="identity", position = "dodge") +  
        ggtitle("Comparison of E-waste generated") +
        scale_fill_brewer(palette = "Set1") +xlab("Year")+
        ylab("Tonnes of E-waste generated") + labs(fill = "Council") +
        theme(axis.text=element_text(size=18), axis.title=element_text(size=22), plot.title = element_text(size=28)
              ,legend.text = element_text(size=16), legend.title = element_text(size=22))
      }, height = 700, width = 700)
  
}

shinyApp(ui, server)


