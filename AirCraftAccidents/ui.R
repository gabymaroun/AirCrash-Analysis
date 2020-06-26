#projet
library('readxl')
library(leaflet)
library(dplyr)
library(ggvis)
if (FALSE) {
    library(RSQLite)
    library(dbplyr)
}
library(shinythemes)
library(shinydashboard)

header<-dashboardHeader(  title = span(icon("plane")," Air Accidents")
)

sidebar<-dashboardSidebar(disable = FALSE, collapsed = TRUE,
                          sidebarMenu(
                              menuItem("Map", tabName = "map", icon = icon("map")),
                              menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
                              menuItem("Data", tabName = "datas", icon = icon("table"))
                              )
)

body <- dashboardBody( 
    tabItems
    (#Map 
        tabItem("map",
                fluidRow(
                    div(class="outer",
                        
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = 60, left = 60 , right ="auto", bottom = "auto",
                                      width = 300, height = "auto",
                                      
                                      h2("Years explorer"),
                                      
                                      dateRangeInput("dates", label = h3("Date range"),start = '1920-01-01',end = '2020-01-10', 
                                                     min = '1918-01-01', max = '2020-01-10',startview ="years" , format = "dd-mm-yyyy")                              )
                        
                    ))
        ),
        
        tabItem("analysis",
                
                fluidRow(
                    column(9,
                           box( width = NULL, status = "success", solidHeader = TRUE,
                                ggvisOutput("plot1"), 
                                valueBox( textOutput("n_crashes"), "Number of crashes selected", 
                                          width = "auto", icon = icon("list-ol"), color = "yellow")
                           )
                           
                    ),
                    
                    column(3,
                           box( title="Filter" ,width = NULL, status = "warning", solidHeader = TRUE,
                                collapsible = TRUE, collapsed = TRUE, background = "navy",
                                
                                sliderInput("fatalities", "Minimum number of fatalities in a crash",
                                            0, 600, 0, step = 30),
                                conditionalPanel("input.plot == 'Scater plot'||input.plot == 'Histogram'||input.plot == 'Density'",
                                                 # Only prompt for threshold when coloring or sizing by superzip
                                                 sliderInput("year", "Crashed Year", 1918, 2020, value = c(1970, 2020),sep=''),
                                ),
                                conditionalPanel("input.plot == 'Boxplot'",
                                                 # Only prompt for threshold when coloring or sizing by superzip
                                                 sliderInput("year1", "Crashed Year", 1918, 2020, value = c(2019,2020),sep=''),
                                ),
                                sliderInput("aboard", "Minimum number of people Aboard",
                                            0, 600, 0, step = 30),
                                conditionalPanel("input.plot == 'Histogram'",
                                                 # Only prompt for threshold when coloring or sizing by superzip
                                                 checkboxInput(inputId = "poly",
                                                               label = strong("Frequence Polygon"),
                                                               value = TRUE)
                                ),
                                conditionalPanel("input.plot == 'Scater plot'",
                                                 # Only prompt for threshold when coloring or sizing by superzip
                                                 checkboxInput(inputId = "smooth",
                                                               label = strong("Smooth layer"),
                                                               value = FALSE)
                                ),
                                textInput("operator", "Search for operators (e.g., Emirates)"),
                                textInput("planeType", "Search for plane types (e.g. Boeing)")
                           ),
                           box( title="Plot Type" ,width = NULL, status = "warning", solidHeader = TRUE,
                                collapsible = TRUE, collapsed = TRUE, background = "navy",
                                selectInput(inputId = "plot","", choices = c("Scater plot","Histogram","Density","Boxplot"), selected = "Scater plot"),
                           )
                           
                    )
                )
        ),
        tabItem("datas",
                fluidRow(
                    DT::dataTableOutput("table")
                )
        )
        
    )
)

dashboardPage(title= "Air Accidents", 
              skin = "yellow",
              header,
              sidebar,
              body
)
