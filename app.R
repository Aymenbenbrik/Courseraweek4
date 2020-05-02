
library(leaflet)
library(shiny)
library(dplyr)
library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
    countriesSP <- getMap(resolution='low')
    #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    #indices$continent   # returns the continent (6 continent model)
    indices$REGION   # returns the continent (7 continent model)
    #indices$ADMIN  #returns country name
    #indices$ISO3 # returns the ISO3 code 
}

covid    <- read.csv(file = "time_series_2019-ncov-Confirmed.csv")
covid$continent = coords2continent(data.frame(lon = covid$Long, lat= covid$Lat))

levels(covid$continent)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Coursera week 2 , 21-03-2020", "Aymen Ben Brik"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("jour",
                        "Evolution du corona au jour :",
                        min = 1,
                        max = 58,
                        value = 58),
            checkboxInput("Africa", "On/Off continent Afrique", value = TRUE),
            checkboxInput("Asia", "On/Off continent Asie", value = TRUE),
            checkboxInput("Antartica", "On/Off continent Antartique", value = TRUE),
            checkboxInput("Australia", "On/Off continent Australie", value = TRUE),
            checkboxInput("Europe", "On/Off continent Europe", value = TRUE),
            checkboxInput("North America", "On/Off continent Amérique du nord", value = TRUE),
            checkboxInput("South America", "On/Off continent Amérique du sud", value = TRUE),
            submitButton("Submit")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Bonjour, voici la propagation du virus par jour"),
            leafletOutput('mymap')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        df = data.frame(Province = covid$Province.State,  Pays = covid$Country.Region, lat= covid$Lat, long=covid$Long ,dernier = covid[,input$jour +4])
        df$Province = as.character(df$Province)
        df$Pays = as.character(df$Pays)
        df$popup = paste(df$Pays,df$Province  , sep = ',')
        
        my_map = df %>% 
            leaflet() %>%
            addTiles() %>%
            addCircleMarkers(popup = df$popup ,color = df$dernier ,radius = sqrt(df$dernier)/10)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
