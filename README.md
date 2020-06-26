---
title: "AirCraft Accidents"
author: "Gaby Maroun"
resource_files:
- airAccs.xlsx
- gomap.js
- styles.css
output:
  html_notebook:
    df_print: paged
    highlight: pygments
    number_sections: yes
    theme: yeti
    toc: yes
    toc_float:
      collapsed: yes
      smooth_scroll: yes
runtime: shiny
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library('readxl')
library(dplyr)
library(ggvis)
if (FALSE) {
  library(RSQLite)
  library(dbplyr)
}
airAccidents <- as.data.frame(read_xlsx("airAccs.xlsx", col_names = TRUE,col_types = c("numeric", "guess","guess", "guess", "guess", "guess", "guess", "numeric", "numeric", "numeric")))


```


# Introduction
  Dans ce document, nous allons décrire, étape par étape, comment créer une application shiny à l'aide d'un tableau de bord shiny et comment comprendre les résultats.

Pour ce faire, nous allons utiliser l'ensemble de données suivant:
```{r table, echo=FALSE}

DT::renderDataTable(airAccidents, options = list(height = 100, width=100))

```
Cet ensemble de données peut être trouvé [ici](https://vincentarelbundock.github.io/Rdatasets/datasets.html). 
Il m'a fallu tellement de temps pour le nettoyer, en ajoutant les données des années depuis 2014 jusqu'à maintenant et en ajoutant également 2 nouvelles colonnes de la longitude et de la latitude de chacune des 6000 lignes.

# Interactive Map
## Leaflet
  Pour utiliser une carte interactive, vous devez installer la bibliothèque de brochures afin de pouvoir utiliser `leaflet ()` côté serveur. Vous pouvez également définir la vue de démarrage avec `setView`. `setMaxBounds` est utilisé pour mettre les limites de la carte. Utilisez `leafletOutput` pour appeler la sortie du côté de l'interface utilisateur.
```{r map}
library(shiny)
library(leaflet)

shinyApp(

  ui = fluidPage(div(class="outer",

                         tags$head(
                           # Include our custom CSS
                                                          includeCSS("styles.css"),
                           includeScript("gomap.js")

                         ),
                    leafletOutput("map", width="100%", height="100%"))

  ),

  server = function(input, output) {
   
  output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
      setView(lng =-20,lat=40, zoom = 2) %>%
      setMaxBounds( lng1=190, lat1=90, lng2=-180, lat2=-90)
    
  })
},

  options = list(height = 500)
)
```
Notez l'utilisation du paramètre `height` pour déterminer la quantité d'espace vertical que l'application intégrée devrait occuper.

## Add ons
  Il est également possible d'ajouter des "Markers" comme dans mon cas ou des "addCircles", "addLegends", ... (vous pouvez consulter [Leaflet for R](https://rstudio.github.io/leaflet/) pour plus d'informations)

Pour que les données de la carte et ses marqueurs restent à jour avec les changements de plage de dates, elles doivent être placées dans un environnement `observe` (plus d'informations sur` observer` et `réactif` dans [shiny Reactive](https: // shiny.rstudio.com/articles/reactivity-overview.html)).
En raison de la grande quantité de données utilisées, nous pouvons utiliser le clustering avec `clusterOptions` pour diminuer le nombre de marqueurs peints sur la carte et cela accélère le résultat.

Donc, côté serveur, il devrait être écrit comme suit,
```{r marker, eval=FALSE}

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

```
N'oubliez pas de `clearMarkerClusters` donc à chaque changement, les marqueurs sont effacés puis repeints.

Du côté de l'utilisateur, nous ajoutons simplement un `dateRangeInput` implémenté dans un` AbsolutePanel` flottant au-dessus de la carte.

Le résultat final ressemblera à quelque chose comme ça,
```{r mapRes, echo=FALSE}
library(shiny)
library(leaflet)

shinyApp(

  ui = fluidPage(
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
                              width = 200, height = 150,
                              
                              h4("Years explorer"),
                              
                              dateRangeInput("dates", label = h5("Date range"),start = '1920-01-01',end = '2020-01-10', 
                                             min = '1918-01-01', max = '2020-01-10',startview ="years" , format = "dd-mm-yyyy")                              )
            
            )),

  server = function(input, output) {
   
  output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
      setView(lng =-20,lat=40, zoom = 1) %>%
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
},

  options = list(height = 500)
)
```
Nous pouvions voir les emplacements des accidents avec précision sur la carte et nous pouvions sentir à quel point cette carte interactive pouvait être conviviale.

On peut constater que la plupart des accidents se produisent dans la partie entre l'amérique du nord et l'amérique du sud. il est donc près du triangle des Bermudes, c'est pourquoi la peur de voler au-dessus de cet endroit est si populaire

# Visualization
  Dans ce qui suit, nous allons décrire comment utiliser `ggvis` à partir de la` library(ggvis) `qui est utilisée pour visualiser les données de plusieurs façons telles que`scater plot`, `histograms`,` barchart`, ` densities`, `polygons`, ... (vous pouvez vérifier le nombre d'entités incluses dans [ggvis basics](https://ggvis.rstudio.com/ggvis-basics.html)).

`ggvis` est un package de visualisation de données génial qui construit des graphiques de données avec une syntaxe similaire à` ggplot2` et crée des tracés interactifs riches comme shiny.

Si vous ne spécifiez pas le type de layer, ggvis utilisera `layer_guess()` pour donner une estimation approximative.

## Interactive Data
  Nous devrons d'abord rendre notre jeu de données interactif afin d'être filtré à chaque changement donc nous devons retourner dans l'environnement `réactif` afin que les données puissent être mises à jour comme indiqué sur les `inputs` et par les widgets.

`grep` est utilisé pour comparer le premier paramètre avec la liste des seconds et retourner leurs index afin de filtrer la table uniquement avec les mots recherchés.

Nous ajoutons au `server.R` le code suivant, si facile à comprendre,
```{r crash, echo=TRUE, eval = FALSE}

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
      
      # Optional: filter by operator
    if (!is.null(input$operator) && input$operator != "") {
      acc <- acc[grep(input$operator, acc$Operator, value = FALSE, ignore.case = TRUE),]
    }
      
    # Optional: filter by plane type
    if (!is.null(input$planeType) && input$planeType != "") {
      acc <- acc[grep( input$planeType,
                acc$`Plane Type`, value = FALSE, ignore.case = TRUE),]
    
    }
      acc <- as.data.frame(acc)
    })
```
(il est préférable d'utiliser un `trycatch()` pour `grep`)

## Scater Plot
  Pour commencer, nous allons travailler sur `layer_points` pour visualiser un graphique scater plot.
 
Nous pourrions spécifier des couleurs avec `fill` et des couleurs de trait avec` stroke`, l'opacité des points avec `fillOpacity`.

`key` est utilisé pour envoyer un message au réactif avec l'id du point survolé (ici` crashes_tooltip`) afin qu'il puisse afficher la `tooltip` des informations.

Nous ajoutons également les widgets suivants qui peuvent ensuite influencer le tracé.

(N.b .; cet `isolate` est utilisé pour rompre la relation réactive avec l'extérieur afin qu'il ne plante pas les données)
```{r scatter, echo=FALSE}
library(ggvis)
shinyApp(

  ui = fluidPage(
                 ggvisOutput("plot1") 

  ),

  server = function(input, output) {
    #just for this case
  crashes <- reactive({
        as.data.frame(airAccidents)
    })

  crashes_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$No)) return(NULL)
    
    # Pick out the crashe with this ID
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
  })
  
  vis %>% bind_shiny("plot1")
  },

  options = list(height = 500)
)
```
Nous pouvons ajouter un `layer_smooths` au nuage de points qui aide à l'analyse. 

Vous pouvez également passer la souris sur les points et voir leurs détails.

Nous pouvons voir comment les points sont si proches de la ligne $y = x$, et c'est parce que dans la plupart des accidents, la gravité est de ~100% donc x (~ Dead) égal y (~ Aboard) et cela montre à quel point les accidents d'avion sont mortels.

## Histogram
  Pour visualiser un histogramme en utilisant `ggvis`, nous utiliserons la fonction` layer_histograms` (`layer_rects` peut également être utilisé). 

Comme nous le verrons, toutes les couches du `ggvis` ont approximativement les mêmes caractéristiques. C'est donc si facile après l'avoir fait pour la première fois de faire les autres.
```{r histo, echo=FALSE}
library(ggvis)
shinyApp(

  ui = fluidPage(
                 ggvisOutput("plot1") 

  ),

  server = function(input, output) {
  
  vis1 <- reactive({
      xvar <- prop("x",as.symbol(names(airAccidents)[2]))
      
      airAccidents %>%
        ggvis(xvar) %>%
          layer_histograms( center = 0, fill:="navy") %>%
          layer_freqpolys( fill := "orange", fillOpacity := .3,
                                 strokeWidth := 3, stroke := "orange")%>%
        add_axis("x", title = names(airAccidents)[2]) %>%
        add_axis("y", title = "Number of Accidents") %>%
        set_options(width ="auto", height = 500)
    })
  
  vis1 %>% bind_shiny("plot1")
  },

  options = list(height = 500)
)
```
Ici aussi, nous avons la possibilité d'ajouter un polygone de fréquence appelé `layer_freqpolys` qui joignent les centres des barres.

Nous pouvons voir que les années 70 et 80 sont les plus brutales et cela vient de la façon dont l'industrie du vol a augmenté au cours de ces décennies et bien sûr au début, elle n'était pas aussi sûre et aussi forte qu'elle l'est de nos jours.([Airplane Timeline](http://www.greatachievements.org/?id=3728))

## Denisty
Pour visualiser la densité en utilisant `ggvis`, nous utiliserons les`layer_densities`.
```{r density, echo=FALSE}
library(ggvis)
shinyApp(

  ui = fluidPage(
                 ggvisOutput("plot1") 

  ),

  server = function(input, output) {
  
  vis2 <- reactive({
      xvar <- prop("x",as.symbol(names(airAccidents)[8]))
      airAccidents %>%
        ggvis(xvar) %>%
        layer_densities(stroke := "orange", fill := "navy") %>%
        add_axis("x", title = names(airAccidents)[8]) %>%
        add_axis("y", title = "Density of Accidents") %>%
        set_options(width ="auto", height = 500)
    })
  
  vis2 %>% bind_shiny("plot1")
  },

  options = list(height = 500)
)
```
On peut en déduire que les accidents avec moins de 50 morts sont les plus denses

## Boxplots
  Pour visualiser les boxplots en utilisant `ggvis`, nous utiliserons les` layer_boxplots`. 

Il est préférable d'utiliser des données aussi petites que possible afin qu'elles soient bien vues et comprises. C'est pourquoi nous filtrerons l'ensemble de données à partir de 2019.
```{r boxp, echo=FALSE}
library(ggvis)
shinyApp(

  ui = fluidPage(
                 ggvisOutput("plot1") 

  ),

  server = function(input, output) {
  
  vis3 <- reactive({
        airAccidents<-filter(airAccidents,format(airAccidents$Date,"%Y") >=2019)
        xvar <- prop("x",as.symbol(names(airAccidents)[7]))
        yvar <- prop("y",as.symbol(names(airAccidents)[8]))
        
       airAccidents %>%
          ggvis(x =xvar, y = yvar
          ) %>%
          layer_boxplots(size := 20, width = .7, strokeOpacity := .7,
                         strokeWidth := 2)%>%
          add_axis("x", title = names(airAccidents)[7]) %>%
          add_axis("y", title = names(airAccidents)[8]) %>%
          set_options(width ="auto", height = 500)
        })
  
  vis3 %>% bind_shiny("plot1")
  },

  options = list(height = 500)
)
```
On peut soustraire de son boxplot que le plus grand nombre d'accidents est fait par des avions de type Boeing depuis 2019. 
Mais cela ne fait pas d'elle la pire compagnie, mais ce nombre est plus grand que les autres parce que Boeing a tellement plus d'avions que toute autre compagnie, ils sont un des premiers constructeur d'avions.([THE MAJOR AIRPLANE MANUFACTURERS AT A GLANCE](https://www.bjtonline.com/business-jet-news/the-major-airplane-manufacturers-at-a-glance))

# Dashboard
  To use dashboard, we have to install `library(shinydashboard)`.
A dashboard page consists of a header `dashboardHeader`, a sidebar `dashboardSidebar` and a body `dashboardBody`. All of them are implemented under the function `dashboardPage`.
Pour utiliser le tableau de bord, nous devons installer `library(shinydashboard)`.

Une page de tableau de bord se compose d'un en-tête `dashboardHeader`, d'une barre latérale `dashboardSidebar` et d'un corps `dashboardBody`. Tous sont implémentés sous la fonction `dashboardPage` de la manière suivante,
```{r dash, eval= FALSE}
dashboardPage(
  dashboardHeader,
  dashboardSidebar,
  dashboardBody
)
```
Dans un `dashboardHeader`, nous pouvons utiliser `title` pour mettre un titre pour la page. Alors que dans `dashboardSidebar`, nous pouvons personnaliser la barre latérale de la page, en utilisant `sidebarMenu` qui lui-même est composé de `menuItems`. Dans le `dashboardBody`, nous pouvons appeler les `menuItems` par leurs identifiants et les mettre dans un `tabItem` pour qu'une relation naisse entre les deux.

Dans une `dashboardPage`, de nombreux panneaux tels que` box` peuvent avoir des couleurs différentes en utilisant `skin`,`color`, `status` ou` background` et cela peut également être interactif.

Vous trouvez tout sur l'apparence d'un `dashboardPage` là dans  [shinydashboard](https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors)

# Conclusion
  Pour conclure, nous avons un excellent moyen de visualiser nos données, une carte interactive avec des marqueurs de chaque accident depuis 1918, un nuage de points interactif qui montre la gravité des accidents, un histogramme avec son polygone de fréquence qui montre les décès par année, un graphique de densité des nombres de décès les plus denses et également un diagramme en boîte comparant les décès pour chaque opérateur.

  Nous avons rendu la création d'une application brillante si simple, mais en réalité, c'est plus compliqué que cela, mais après avoir passé beaucoup de temps à travailler sur chaque mot et à comprendre chaque commande, sachant que cela prendra tellement de temps, mais ma motivation était que R est quelque chose qui durera si longtemps avec moi et si je veux l'apprendre et l'utiliser, je devrai profiter au maximum de l'expérience.
