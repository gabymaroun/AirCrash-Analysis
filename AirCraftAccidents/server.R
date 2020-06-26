#projet
library(leaflet)
library("readxl")
library(dplyr)
library(ggvis)
if (FALSE) {
    library(RSQLite)
    library(dbplyr)
}
airAccidents <- read_xlsx("../AirCraftAccidents/airAccs.xlsx", col_names = TRUE,col_types = c("numeric", "guess","guess", "guess", "guess", "guess", "guess", "numeric", "numeric", "numeric"))

airAccidents$Date <- as.Date(airAccidents$Date, format= "%Y-%m-%d")

function(input, output, session) {
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            setView(lng =-20,lat=40, zoom = 2) %>%
            setMaxBounds( lng1=190, lat1=90, lng2=-180, lat2=-90)
        
    })
    
    
    observe({
        daterng <-range(input$dates[1],input$dates[2])
        
        airAccidents1<-airAccidents[ as.Date(airAccidents$Date,"%d-%m-%Y") >=as.character(daterng[1]) &
                                         as.Date(airAccidents$Date,"%d-%m-%Y") <= as.character(daterng[2]),]
        
        leafletProxy("map", data =  airAccidents1) %>%
            clearMarkerClusters() %>%
            addMarkers(lng = airAccidents1$Longitude, lat =airAccidents1$Latitude,
                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                       labelOptions = labelOptions(noHide = F,direction = 'auto'),
                       popup = paste("<h4><b>Operator:</b> ",airAccidents1$Operator, "</h4>",
                                     "<b> ",airAccidents1$Location,"</b>, ",
                                     format(airAccidents1$Date, "%d/%m/%Y"),"<br>",
                                     "Fatalities:",airAccidents1$Dead," / ",
                                     "Aboard:", airAccidents1$Aboard,"<br>",
                                     "Gravity:",round((as.integer(airAccidents1$Dead)/as.integer(airAccidents1$Aboard))*100,2),"%  &nbsp &nbsp &nbsp &nbsp",
                                     "Plane Type:", airAccidents$`Plane Type`
                       )
            )  
    })
    
    # Filter the crashes, returning a data frame
    crashes <- reactive({

        fatalities <- input$fatalities
        aboard <- input$aboard
        
        if(input$plot=="Boxplot"){
            minyear <- input$year1[1]
            maxyear <- input$year1[2]}
        else{
            minyear <- input$year[1]
            maxyear <- input$year[2]
        }
        
        # Apply filters
        acc <- airAccidents %>%
            filter(
                Dead >= fatalities,
                format(airAccidents$Date,"%Y") >=minyear &
                    format(airAccidents$Date,"%Y") <= maxyear,
                Aboard >= aboard)
        
            result <- tryCatch({
                # Optional: filter by operator
                if (!is.null(input$operator) && input$operator != "") {
                acc <- acc[grep(input$operator,
                                acc$Operator, value = FALSE, ignore.case = TRUE),]}
                # Optional: filter by plane type
                if (!is.null(input$planeType) && input$planeType != "") {
                    acc <- acc[grep(input$planeType,
                                    acc$`Plane Type`, value = FALSE, ignore.case = TRUE),]
                }
            }, warning = function(w) {
                message(paste("name caused a warning:", acc))
                return(NULL)           
            }, error = function(e) {
                message(paste("name does not seem to exist:", acc))
                return(NA)            
            }, finally = {
                acc
                
            })
            
        acc <- as.data.frame(acc)
    })
    
    
    # Function for generating tooltip text
    crashes_tooltip <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.null(x$No)) return(NULL)
        
        # Pick out the crash with this ID
        airAccidents <- isolate(crashes())
        crashe <- airAccidents[airAccidents$No == x$No, ]
        
        paste0("<body color='black'><h4><b>Operator:</b> ",crashe$Operator, "</h4>",
               "<b> ",crashe$Location,"</b>, ",
               format(crashe$Date, "%d/%m/%Y"),"<br>",
               "Gravity:",round((as.integer(crashe$Dead)/as.integer(crashe$Aboard))*100,2),"%  &nbsp &nbsp &nbsp &nbsp",
               "Plane Type:", crashe$`Plane Type`,"</body>"
        )
    }
    
    
    
    vis <- reactive({
        xvar <- prop("x",as.symbol(names(airAccidents)[9]))
        yvar <- prop("y",as.symbol(names(airAccidents)[8]))
        
        if(input$smooth){
            crashes %>%
                ggvis(x = xvar, y = yvar, fill=~Dead, fill:="yellow",
                      stroke :="black") %>%
                layer_points(size := 50, size.hover := 200,
                             fillOpacity := 0.2, fillOpacity.hover := 0.5,
                             key :=~No) %>% 
                layer_smooths(se = TRUE, fill:="red")%>%
                add_tooltip(crashes_tooltip, "hover") %>%
                add_axis("x", title = names(airAccidents)[9]) %>%
                add_axis("y", title = names(airAccidents)[8]) %>%
                set_options(width ="auto", height = 500)
        }else{
            crashes %>%
                ggvis(x = xvar, y = yvar, fill=~Dead, fill:="yellow",
                      stroke :="black") %>%
                layer_points(size := 50, size.hover := 200,
                             fillOpacity := 0.2, fillOpacity.hover := 0.5,
                             key :=~No) %>% 
                add_tooltip(crashes_tooltip, "hover") %>%
                add_axis("x", title = names(airAccidents)[9]) %>%
                add_axis("y", title = names(airAccidents)[8]) %>%
                set_options(width ="auto", height = 500)
        }
    })
    
    vis1 <- reactive({
        xvar <- prop("x",as.symbol(names(airAccidents)[2]))
        
        if(input$poly){
            crashes %>%
                ggvis(xvar) %>%
                layer_histograms( center = 0, fill:="navy") %>%
                layer_freqpolys( fill := "orange", fillOpacity := .3,
                                 strokeWidth := 3, stroke := "orange")%>%
                add_axis("x", title = names(airAccidents)[2]) %>%
                add_axis("y", title = "Number of Accidents") %>%
                set_options(width ="auto", height = 500)
        }
        else{
            crashes %>%
                ggvis(xvar) %>%
                layer_histograms( center = 0, fill:="navy") %>%
                add_axis("x", title = names(airAccidents)[2]) %>%
                add_axis("y", title = "Number of Accidents") %>%
                set_options(width ="auto", height = 500)
        }
    })
    
    vis2 <- reactive({
        xvar <- prop("x",as.symbol(names(airAccidents)[8]))
        crashes %>%
            ggvis(xvar) %>%
            layer_densities(stroke := "orange", fill := "navy") %>%
            add_axis("x", title = names(airAccidents)[8]) %>%
            add_axis("y", title = "Density of Accidents") %>%
            set_options(width ="auto", height = 500)
    })
    
    vis3 <- reactive({
        xvar <- prop("x",as.symbol(names(airAccidents)[7]))
        yvar <- prop("y",as.symbol(names(airAccidents)[8]))
        
        crashes %>%
            ggvis(x = xvar, y = yvar) %>%
            layer_boxplots(size := 20, width = .7, strokeOpacity := .7,
                           strokeWidth := 2)%>%
            add_axis("x", title = names(airAccidents)[7]) %>%
            add_axis("y", title = names(airAccidents)[8]) %>%
            set_options(width ="auto", height = 500)
    })
    
    observe({
        plot<-input$plot
        switch(plot,
               "Scater plot" = vis %>% bind_shiny("plot1"),
               "Histogram" = vis1 %>% bind_shiny("plot1"),
               "Density" = vis2 %>% bind_shiny("plot1"),
               "Boxplot" = vis3 %>% bind_shiny("plot1")
        )
        
    })
    
    output$n_crashes <- renderText({ nrow(crashes()) })
    
    output$table <-DT::renderDataTable({DT::datatable(airAccidents, options = list(height = 100, width=100))})

}