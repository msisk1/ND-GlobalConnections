library(shiny)
library(leaflet)
library(shinythemes)
library(sf)
library(dplyr)

#todo:
#  Give the US a grey fill


load("FullList_all.rData")
full.list.map <- st_simplify(full.list.map, dTolerance = .04)
# full.list.map <- full.list.map %>%
#     mutate(tot_map = if_else(condition = (tot_map == 0), true = NA_integer_,false = tot_map))

var.choices = c("All Faculty" = "All", 
                "Science Faculty" = "Science",
                "Engineering Faculty" = "Engineering",
                "Arts and Letters Faculty" = "Arts and Letters",
                "Social Science Faculty" = "Social Science",
                "Library Faculty" = "Library",
                "Keough Faculty" = "Keough")
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # Application title
                titlePanel("Global Connectedness of Notre Dame Research"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        radioButtons(inputId = "usedVar",
                                     label = "Type:",
                                     choices = var.choices,
                                     
                        )
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        leafletOutput("map", height = "95vh")
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    active.layer <- eventReactive(c(input$usedVar), {
        temper <- full.list.map %>%
            mutate(active = get(input$usedVar))
        temper <- temper%>%
            mutate(active =  if_else(condition = (admin == "United States of America"), true = NA_integer_, false = as.integer(active)))
        return(temper)
        
        
    }, ignoreNULL = FALSE)#end points.orig
    output$map <- renderLeaflet({
        lab.val <- active.layer()[active.layer()$admin == "United States of America",input$usedVar]%>%
            st_set_geometry(NULL)%>%
            pull(input$usedVar)
        pal.all <- colorNumeric(
            palette = "viridis",
            domain = c(1:max(active.layer()$active, na.rm = T)),
            
            na.color = NA)
        
        leaflet() %>%
            addTiles(options = tileOptions(noWrap = TRUE))%>%
            setView(lat =0, lng = 0, zoom =2 )%>%
            addLabelOnlyMarkers(lat = 39.833333, lng = -100, label = HTML(paste0("<center><b>",lab.val, " Domestic <br> Co-authors")),
                                labelOptions = labelOptions(noHide = T, textOnly = TRUE,  direction = 'center'))%>%
            addPolygons(data = active.layer(), label = ~popup, weight = .5, color = "white", smoothFactor = 0.2, fillOpacity = 1,            # addPolygons(data = active.layer(), label = ~popup, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                        fillColor = ~pal.all(active.layer()$active))%>%
            addLegend(pal = pal.all, values = c(1:max(active.layer()$active, na.rm = T)), na.label = F, position = "bottomright",title = "# of global co-authors",
            )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
