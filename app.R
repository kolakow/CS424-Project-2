#Pat Kolakowski, pkolak3@uic.edu
#Project 2: Raw Power
#CS 424, Spring 2020
#Data sourced from: https://www.epa.gov/egrid/download-data
#files used: eGRID2018v2.xlsx and eGRID2000_plan.xlsx, eGRID2010_plan.xlsx from eGRID historical files (1996 - 2016)

#libraries to include
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(stringr)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(leaflet.extras)
library(scales)

data2000 <- read.csv("egrid2000-final.csv", header = TRUE, stringsAsFactors = FALSE) #Read in data
data2010 <- read.csv("egrid2010-final.csv", header = TRUE, stringsAsFactors = FALSE) #Read in data
data2018 <- read.csv("egrid2018-final.csv", header = TRUE, stringsAsFactors = FALSE) #Read in data
fullStateName <- state.name[match(data2000$STATE,state.abb)] #Convert abbreviated states to full name

#Color palette for different energy sources
pal <- colorFactor(c("#000000", "#e6194b", "#9A6324", "#3cb44b", "#000075", "LIGHTBLUE", "#469990", "#ffe119", "RED", "PURPLE", "MAGENTA", "ORANGE"), levels = c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewables", "Nonrenewables"), ordered = FALSE)

#List of items used for all the check box groups
checkboxList <- list("All" = "All", "Biomass" = "Biomass", "Coal" = "Coal", "Gas" = "Gas", "Geothermal" = "Geothermal", "Hydro" = "Hydro", "Nonrenewables" = "Nonrenewables", "Nuclear" = "Nuclear", "Oil" = "Oil", "Other" = "Other", "Renewables" = "Renewables", "Solar" = "Solar", "Wind" = "Wind")


ui <- dashboardPage(
    dashboardHeader(title = "CS 424: Project 2\n"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                    sidebarMenu(
                        id = "tab",
                        menuItem("Illinois Leaflet Map", tabName = "1"),
                        menuItem("Compare Leaflet Maps", tabName = "2"),
                        menuItem("US Leaflet Map", tabName = "3"),
                        menuItem("About", tabName = "4")
                    ),
                    uiOutput("tabMenu")
                    ),
dashboardBody(
    tabItems(
        tabItem(tabName = "1", #Page 1, Illinois 2018 Leaflet Map
          fluidPage(
                box(title = "Illinois 2018 Leaflet Map", status = "primary", width = 14,
                        leafletOutput("ilMap", height = 850)
                    ),
                )
               ),
    tabItem(tabName = "2", #Page 2, Compare Leaflet Maps
        fluidRow(
            column(6,
                fluidRow(
                tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                  box(status = "primary", width = 12,
                    column(4,
                        fluidRow( #Year selection dropdown
                            selectInput("yearSelect1", label = "", choices = list("2000", "2010", "2018"), selected = "2000"),
                            )
                    ),
                    column(4,
                        fluidRow( #State selection dropdown
                            selectInput("stateSelect1", label = "", choices = fullStateName, selected = "Illinois"),
                            )
                        ),
                    column(4,
                        fluidRow( #Layer selection dropdown
                            selectInput("layerSelect1", label = "", choices = list("Default Mode", "B&W Mode", "Topography Mode"), selected = "Default"),
                            )
                        ),
                    leafletOutput("map1", height = 800)
                  )
                 )
            ),
            column(6,
                fluidRow(
                tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                  box(status = "primary", width = 12,
                    column(4,
                        fluidRow( #Year selection dropdown
                            selectInput("yearSelect2", label = "", choices = list("2000", "2010", "2018"), selected = "2018"),
                            )
                    ),
                    column(4,
                        fluidRow( #State selection dropdown
                            selectInput("stateSelect2", label = "", choices = fullStateName, selected = "Illinois"),
                            )
                        ),
                    column(4,
                        fluidRow( #Layer selection dropdown
                            selectInput("layerSelect2", label = "", choices = list("Default Mode", "B&W Mode", "Topography Mode"), selected = "Default"),
                            )
                        ),
                    leafletOutput("map2", height = 800)
                  )
                 )
            ),
            )
           ),
    tabItem(tabName = "3", #Page 3, US Leaflet Map
        fluidPage(
              box(status = "primary", width = 12,
              column(4,
                fluidRow( #Year selection dropdown
                    selectInput("yearSelect3", label = " ", choices = list("2000", "2010", "2018"), selected = "2018"),
                    )
                ),
             column(1,
                fluidRow(
                    )
                ),
                column(7,
                  fluidRow( #Range selection sliders
                      sliderInput("rangeSelect3", label = "", min = 0, max = 25000000, value = range(data2018$TOTAL.GENERATION), step = 100000),
                      )
                  ),
                      leafletOutput("USMap", height = 800)
                
            )
            )
         ),
    tabItem(tabName = "4", #Page 4, About page
        h3("Project 2: Raw Power"),
        h4("by Pat Kolakowski (pkolak3@uic.edu)"),
        h4("for UIC CS 424, Spring 2020"),
        h4("Data sourced from https://www.epa.gov/egrid/download-data/"),
        h4("Information on app creation and installation can be found at: https://pkolak3.people.uic.edu/project2.html")
        )
    )
)
)


server <- function(input, output, session) {
    output$tabMenu <- renderUI({ #Render UI based on tab selected

    #Page 1, show Illinois 2018 leaflet
    if(input$tab == "1") {
        dyn_ui <- list(checkboxGroupInput("checkGroup1", label = h5("Energy Sources"), #Checkbox with energy source options
                    choices = checkboxList,
                    selected = "All"),
                    actionButton("reset_button1","Reset View")) #Reset button
        }
    
    #Page 2, Leaflet Comparison
    if(input$tab == "2") {
        fullStateName <- state.name[match(data2000$STATE,state.abb)]
        dyn_ui <- list(
                checkboxGroupInput("checkGroup2", label = h5("Energy Sources"), #Checkbox with energy source options
                            choices = checkboxList,
                            selected = "All"),
                            actionButton("reset_button2","Reset View") #Reset button
                )
        updateSelectizeInput(session, 'stateSelect1', choices = fullStateName, selected = "Illinois", server = TRUE) #Preselected options
        updateSelectizeInput(session, 'stateSelect2', choices = fullStateName, selected = "Illinois", server = TRUE)
        }
    
    #Page 3, US Leaflet Map
    if(input$tab == "3") {
        dyn_ui <- list(checkboxGroupInput("checkGroup3", label = h5("Energy Sources"), #Checkbox with energy source options
                    choices = checkboxList),
                    actionButton("reset_button1","Reset View")) #Reset button
        }
    
    #Page 4, About page
    if(input$tab == "4") {
        dyn_ui <- list()
    }
    
    return(dyn_ui)
    })

    #Leaflet on page 1
    output$ilMap <- renderLeaflet({
        data <- data2018[data2018$STATE== "IL",] #Filter data down to Illinois
        value <- cat(input$checkGroup1) #Grab energy sources selected

        map <- leaflet(data) %>% #Create base map
               addTiles() %>%
               setView(-89.044, 39.800, zoom = 7)
               
               #Inefficiently add markers for each energy source selected
               if("Coal" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                          lng = data$LON, lat = data$LAT,
                          color = "#000000",
                          radius = ~ifelse(COAL != 0, 10, 0),
                          stroke = FALSE,
                          fillOpacity = ~ifelse(COAL != 0, .5, 0),
                          popup = ~ifelse(COAL != 0, paste(data$PLANT, "<br>", " % of Coal production: ", data$PERCENT.COAL, "<br>", "Energy Generated: ", data$COAL, " mWh", sep = ""), paste(""))
                          )
               }
               if("Oil" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .001,
                       color = "#e6194b",
                       radius = ~ifelse(OIL != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(OIL != 0, .5, 0),
                       popup = ~ifelse(OIL != 0, paste(data$PLANT, "<br>",  " % of Oil production: ", data$PERCENT.OIL, "<br>", "Energy Generated: ", data$OIL, " mWh", sep = ""), paste(""))
                       )
               }
               if("Gas" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .002,
                       color = "#9A6324",
                       radius = ~ifelse(GAS != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(GAS != 0, .5, 0),
                       popup = ~ifelse(GAS != 0, paste(data$PLANT, "<br>",  " % of Gas production: ", data$PERCENT.GAS, "<br>", "Energy Generated: ", "Energy Generated: ", data$GAS, " mWh", sep = ""), paste(""))
                       )
               }
               if("Nuclear" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .003,
                       color = "GREEN",
                       radius = ~ifelse(NUCLEAR != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(NUCLEAR != 0, .5, 0),
                       popup = ~ifelse(NUCLEAR != 0, paste(data$PLANT, "<br>", " % of Nuclear production: ", data$PERCENT.OIL, "<br>", "Energy Generated: ", data$OIL, " mWh", sep = ""), paste("")
                       )
                   )
               }
               if("Hydro" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .004,
                       color = "#000075",
                       radius = ~ifelse(HYDRO != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(HYDRO != 0, .5, 0),
                       popup = ~ifelse(HYDRO != 0, paste(data$PLANT, "<br>", " % of Hydro production: ", data$PERCENT.HYDRO, "<br>", "Energy Generated: ", data$HYDRO, " mWh", sep = ""), paste("")
                       )
                   )
               }
               if("Biomass" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                        lng = data$LON, lat = data$LAT + .005,
                        color = "LIGHTBLUE",
                        radius = ~ifelse(BIOMASS != 0, 10, 0),
                        stroke = FALSE,
                        fillOpacity = ~ifelse(BIOMASS != 0, .75, 0),
                        popup = ~ifelse(BIOMASS != 0, paste(data$PLANT, "<br>", " % of Biomass production: ", data$PERCENT.BIOMASS, "<br>", "Energy Generated: ", data$BIOMASS, " mWh", sep = ""), paste("")
                        )
                   )
               }
               if("Wind" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .006,
                       color = "#469990",
                       radius = ~ifelse(WIND != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(WIND != 0, .5, 0),
                       popup = ~ifelse(WIND != 0, paste(data$PLANT, "<br>", " % of Wind production: ", data$PERCENT.WIND,  "<br>", "Energy Generated: ", data$WIND, " mWh", sep = ""), paste("")
                       )
                   )
               }
               if("Solar" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .007,
                       color = "#ffe119",
                       radius = ~ifelse(SOLAR != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(SOLAR != 0, .75, 0),
                       popup = ~ifelse(SOLAR != 0, paste(data$PLANT, "<br>", " % of Solar production: ", data$PERCENT.SOLAR, "<br>", "Energy Generated: ", data$SOLAR, " mWh", sep = ""), paste(""))
                   )
               }
               if("Geothermal" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .008,
                       color = "PINK",
                       radius = ~ifelse(GEOTHERMAL != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(GEOTHERMAL != 0, .5, 0),
                       popup = ~ifelse(GEOTHERMAL != 0, paste(data$PLANT, "<br>", " % of Geothermal production: ", data$PERCENT.GEOTHERMAL, "<br>", "Energy Generated: ", data$GEOTHERMAL, " mWh", sep = ""), paste(""))
                   )
               }
               if("Other" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .009,
                       color = "PURPLE",
                       radius = ~ifelse(OTHER != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(OTHER != 0, .5, 0),
                       popup = ~ifelse(OTHER != 0, paste(data$PLANT, "<br>", " % of Other production: ", data$PERCENT.OTHER, "<br>", "Energy Generated: ", data$OTHER, " mWh", sep = ""), paste(""))
                   )
               }
               if("Renewables" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .01,
                       color = "MAGENTA",
                       radius = ~ifelse(TOTAL.RENEWABLE != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(TOTAL.RENEWABLE != 0, .5, 0),
                       popup = ~ifelse(TOTAL.RENEWABLE != 0, paste(data$PLANT, "<br>", " % of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "Energy Generated: ", data$TOTAL.RENEWABLE, " mWh", sep = ""), paste(""))
                   )
               }
               if("Nonrenewables" %in% input$checkGroup1 || "All" %in% input$checkGroup1) {
                   map <- map %>% addCircleMarkers(
                       lng = data$LON, lat = data$LAT + .011,
                       color = "ORANGE",
                       radius = ~ifelse(TOTAL.NONRENEWABLE != 0, 10, 0),
                       stroke = FALSE,
                       fillOpacity = ~ifelse(TOTAL.NONRENEWABLE != 0, .5, 0),
                       popup = ~ifelse(TOTAL.NONRENEWABLE != 0, paste(data$PLANT, "<br>", " % of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, "<br>", "Energy Generated: ", data$TOTAL.NONRENEWABLE, " mWh", sep = ""), paste(""))
                   )
               }
               

            #Add values to legend depending on check boxes selected
            if("All" %in% input$checkGroup1) {
                map %>%
                addLegend("bottomright", pal = pal, values = c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewables", "Nonrenewables"), title = "Energy Sources")
            } else {
                map %>%
                addLegend("bottomright", pal = pal, values = input$checkGroup1, label = input$checkGroup1, title = "Energy Sources")
            }
    })
    
    #Used to reset settings on page 1, Illinois Leaflet Map
    observeEvent(input$reset_button1, {
        leafletProxy("ilMap") %>% setView(-89.044, 39.800, zoom = 7)
    })
    
    #Left map on page 2, compare leaflet maps
    output$map1 <- renderLeaflet({
        data <- NULL
        stateSelected <- state.abb[match(input$stateSelect1,state.name)] #Convert state name back to abbreviated version
        
        #Select year data corresponding to user selection
        if("2010" %in% input$yearSelect1) {
            data <- data2010[data2010$STATE == stateSelected,]
        } else if("2018" %in% input$yearSelect1) {
            data <- data2018[data2018$STATE == stateSelected,]
        } else {
            data <- data2000[data2000$STATE == stateSelected,]
        }
        
        #Filter map tile according to user selection
        if("Topography Mode" %in% input$layerSelect1) {
            map <- leaflet(data) %>% addProviderTiles("OpenTopoMap")
        } else if("B&W Mode" %in% input$layerSelect1) {
            map <- leaflet(data) %>% addProviderTiles(providers$Stamen.Toner)
        } else {
            map <- leaflet(data) %>% addTiles()
        }

        #Inefficiently add markers for each energy source selected
       if("Coal" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
                  lng = data$LON, lat = data$LAT,
                  color = "#000000",
                  radius = ~ifelse(COAL != 0, 10, 0),
                  stroke = FALSE,
                  fillOpacity = ~ifelse(COAL != 0, .5, 0),
                  popup = ~ifelse(COAL != 0, paste(data$PLANT, "<br>", " % of Coal production: ", data$PERCENT.COAL, "<br>", "Energy Generated by Coal: ", data$COAL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
                  )
       }
       if("Oil" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .001,
               color = "#e6194b",
               radius = ~ifelse(OIL != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(OIL != 0, .5, 0),
               popup = ~ifelse(OIL != 0, paste(data$PLANT, "<br>",  " % of Oil production: ", data$PERCENT.OIL, "<br>", "Energy Generated by Oil: ", data$OIL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
               )
       }
       if("Gas" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .002,
               color = "#9A6324",
               radius = ~ifelse(GAS != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(GAS != 0, .5, 0),
               popup = ~ifelse(GAS != 0, paste(data$PLANT, "<br>",  " % of Gas production: ", data$PERCENT.GAS, "<br>", "Energy Generated by Gas: ", data$GAS, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
               )
       }
       if("Nuclear" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .003,
               color = "GREEN",
               radius = ~ifelse(NUCLEAR != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(NUCLEAR != 0, .5, 0),
               popup = ~ifelse(NUCLEAR != 0, paste(data$PLANT, "<br>", " % of Nuclear production: ", data$PERCENT.OIL, "<br>", "Energy Generated by Nuclear: ", data$OIL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste("")
               )
           )
       }
       if("Hydro" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .004,
               color = "#000075",
               radius = ~ifelse(HYDRO != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(HYDRO != 0, .5, 0),
               popup = ~ifelse(HYDRO != 0, paste(data$PLANT, "<br>", " % of Hydro production: ", data$PERCENT.HYDRO, "<br>", "Energy Generated by Hydro: ", data$HYDRO, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste("")
               )
           )
       }
       if("Biomass" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .005,
                color = "LIGHTBLUE",
                radius = ~ifelse(BIOMASS != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(BIOMASS != 0, .75, 0),
                popup = ~ifelse(BIOMASS != 0, paste(data$PLANT, "<br>", " % of Biomass production: ", data$PERCENT.BIOMASS, "<br>", "Energy Generated by Biomass: ", data$BIOMASS, "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, " mWh", sep = ""), paste("")
                )
           )
       }
       if("Wind" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .006,
               color = "#469990",
               radius = ~ifelse(WIND != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(WIND != 0, .5, 0),
               popup = ~ifelse(WIND != 0, paste(data$PLANT, "<br>", " % of Wind production: ", data$PERCENT.WIND,  "<br>", "Energy Generated by Wind: ", data$WIND, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste("")
               )
           )
       }
       if("Solar" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .007,
               color = "#ffe119",
               radius = ~ifelse(SOLAR != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(SOLAR != 0, .75, 0),
               popup = ~ifelse(SOLAR != 0, paste(data$PLANT, "<br>", " % of Solar production: ", data$PERCENT.SOLAR, "<br>", "Energy Generated by Solar: ", data$SOLAR, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
           )
       }
       if("Geothermal" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .008,
               color = "PINK",
               radius = ~ifelse(GEOTHERMAL != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(GEOTHERMAL != 0, .5, 0),
               popup = ~ifelse(GEOTHERMAL != 0, paste(data$PLANT, "<br>", " % of Geothermal production: ", data$PERCENT.GEOTHERMAL, "<br>", "Energy Generated by Geothermal: ", data$GEOTHERMAL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
           )
       }
       if("Other" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .009,
               color = "PURPLE",
               radius = ~ifelse(OTHER != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(OTHER != 0, .5, 0),
               popup = ~ifelse(OTHER != 0, paste(data$PLANT, "<br>", " % of Other production: ", data$PERCENT.OTHER, "<br>", "Energy Generated by Other sources: ", data$OTHER, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
           )
       }
       if("Renewables" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .01,
               color = "MAGENTA",
               radius = ~ifelse(TOTAL.RENEWABLE != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(TOTAL.RENEWABLE != 0, .5, 0),
               popup = ~ifelse(TOTAL.RENEWABLE != 0, paste(data$PLANT, "<br>", "Renewable Energy Generated: ", data$TOTAL.RENEWABLE, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
           )
       }
       if("Nonrenewables" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
           map <- map %>% addCircleMarkers(
               lng = data$LON, lat = data$LAT + .011,
               color = "ORANGE",
               radius = ~ifelse(TOTAL.NONRENEWABLE != 0, 10, 0),
               stroke = FALSE,
               fillOpacity = ~ifelse(TOTAL.NONRENEWABLE != 0, .5, 0),
               popup = ~ifelse(TOTAL.NONRENEWABLE != 0, paste(data$PLANT, "<br>", "Non-Renewable Energy Generated: ", data$TOTAL.NONRENEWABLE, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, " mWh", sep = ""), paste(""))
           )
       }
       
       #Add values to legend depending on check boxes selected
        if("All" %in% input$checkGroup2) {
            map %>%
            addLegend("bottomright", pal = pal, values = c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", "Non-Renewable"), title = "Energy Sources")
        } else {
            map %>%
            addLegend("bottomright", pal = pal, values = input$checkGroup2, label = input$checkGroup2, title = "Energy Sources")
        }
    })
    
    #Right map on page 2, compare leaflet maps
    output$map2 <- renderLeaflet({
        data <- NULL
        stateSelected <- state.abb[match(input$stateSelect2,state.name)] #Convert state name back to abbreviated version
        
        #Select year data corresponding to user selection
        if("2010" %in% input$yearSelect2) {
            data <- data2010[data2010$STATE == stateSelected,]
        } else if("2018" %in% input$yearSelect2) {
            data <- data2018[data2018$STATE == stateSelected,]
        } else {
            data <- data2000[data2000$STATE == stateSelected,]
        }
        
        #Filter map tile according to user selection
        if("Topography Mode" %in% input$layerSelect2) {
            map <- leaflet(data) %>% addProviderTiles("OpenTopoMap")
        } else if("B&W Mode" %in% input$layerSelect2) {
            map <- leaflet(data) %>% addProviderTiles(providers$Stamen.Toner)
        } else {
            map <- leaflet(data) %>% addTiles()
        }
        
        #Inefficiently add markers for each energy source selected
        if("Coal" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                   lng = data$LON, lat = data$LAT,
                   color = "#000000",
                   radius = ~ifelse(COAL != 0, 10, 0),
                   stroke = FALSE,
                   fillOpacity = ~ifelse(COAL != 0, .5, 0),
                   popup = ~ifelse(COAL != 0, paste(data$PLANT, "<br>", " % of Coal production: ", data$PERCENT.COAL, "<br>", "Energy Generated by Coal: ", data$COAL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
                   )
        }
        if("Oil" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .001,
                color = "#e6194b",
                radius = ~ifelse(OIL != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(OIL != 0, .5, 0),
                popup = ~ifelse(OIL != 0, paste(data$PLANT, "<br>",  " % of Oil production: ", data$PERCENT.OIL, "<br>", "Energy Generated by Oil: ", data$OIL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
                )
        }
        if("Gas" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .002,
                color = "#9A6324",
                radius = ~ifelse(GAS != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(GAS != 0, .5, 0),
                popup = ~ifelse(GAS != 0, paste(data$PLANT, "<br>",  " % of Gas production: ", data$PERCENT.GAS, "<br>", "Energy Generated by Gas: ", data$GAS, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
                )
        }
        if("Nuclear" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .003,
                color = "GREEN",
                radius = ~ifelse(NUCLEAR != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(NUCLEAR != 0, .5, 0),
                popup = ~ifelse(NUCLEAR != 0, paste(data$PLANT, "<br>", " % of Nuclear production: ", data$PERCENT.OIL, "<br>", "Energy Generated by Nuclear: ", data$OIL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste("")
                )
            )
        }
        if("Hydro" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .004,
                color = "#000075",
                radius = ~ifelse(HYDRO != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(HYDRO != 0, .5, 0),
                popup = ~ifelse(HYDRO != 0, paste(data$PLANT, "<br>", " % of Hydro production: ", data$PERCENT.HYDRO, "<br>", "Energy Generated by Hydro: ", data$HYDRO, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste("")
                )
            )
        }
        if("Biomass" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                 lng = data$LON, lat = data$LAT + .005,
                 color = "LIGHTBLUE",
                 radius = ~ifelse(BIOMASS != 0, 10, 0),
                 stroke = FALSE,
                 fillOpacity = ~ifelse(BIOMASS != 0, .75, 0),
                 popup = ~ifelse(BIOMASS != 0, paste(data$PLANT, "<br>", " % of Biomass production: ", data$PERCENT.BIOMASS, "<br>", "Energy Generated by Biomass: ", data$BIOMASS, "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, " mWh", sep = ""), paste("")
                 )
            )
        }
        if("Wind" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .006,
                color = "#469990",
                radius = ~ifelse(WIND != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(WIND != 0, .5, 0),
                popup = ~ifelse(WIND != 0, paste(data$PLANT, "<br>", " % of Wind production: ", data$PERCENT.WIND,  "<br>", "Energy Generated by Wind: ", data$WIND, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste("")
                )
            )
        }
        if("Solar" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .007,
                color = "#ffe119",
                radius = ~ifelse(SOLAR != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(SOLAR != 0, .75, 0),
                popup = ~ifelse(SOLAR != 0, paste(data$PLANT, "<br>", " % of Solar production: ", data$PERCENT.SOLAR, "<br>", "Energy Generated by Solar: ", data$SOLAR, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
            )
        }
        if("Geothermal" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .008,
                color = "PINK",
                radius = ~ifelse(GEOTHERMAL != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(GEOTHERMAL != 0, .5, 0),
                popup = ~ifelse(GEOTHERMAL != 0, paste(data$PLANT, "<br>", " % of Geothermal production: ", data$PERCENT.GEOTHERMAL, "<br>", "Energy Generated by Geothermal: ", data$GEOTHERMAL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
            )
        }
        if("Other" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .009,
                color = "PURPLE",
                radius = ~ifelse(OTHER != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(OTHER != 0, .5, 0),
                popup = ~ifelse(OTHER != 0, paste(data$PLANT, "<br>", " % of Other production: ", data$PERCENT.OTHER, "<br>", "Energy Generated by Other sources: ", data$OTHER, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
            )
        }
        if("Renewables" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .01,
                color = "MAGENTA",
                radius = ~ifelse(TOTAL.RENEWABLE != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(TOTAL.RENEWABLE != 0, .5, 0),
                popup = ~ifelse(TOTAL.RENEWABLE != 0, paste(data$PLANT, "<br>", "Renewable Energy Generated: ", data$TOTAL.RENEWABLE, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
            )
        }
        if("Nonrenewables" %in% input$checkGroup2 || "All" %in% input$checkGroup2) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .011,
                color = "ORANGE",
                radius = ~ifelse(TOTAL.NONRENEWABLE != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(TOTAL.NONRENEWABLE != 0, .5, 0),
                popup = ~ifelse(TOTAL.NONRENEWABLE != 0, paste(data$PLANT, "<br>", "Non-Renewable Energy Generated: ", data$TOTAL.NONRENEWABLE, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, " mWh", sep = ""), paste(""))
            )
        }
        
        #Add values to legend depending on check boxes selected
         if("All" %in% input$checkGroup2) {
             map %>%
             addLegend("bottomright", pal = pal, values = c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", "Non-Renewable"), title = "Energy Sources")
         } else {
             map %>%
             addLegend("bottomright", pal = pal, values = input$checkGroup2, label = input$checkGroup2, title = "Energy Sources")
         }
    })
    
    #Used to reset settings on page 2, Compare Leaflet Maps
    observeEvent(input$reset_button2, {
        updateSelectizeInput(session, 'stateSelect1', choices = fullStateName, selected = "Illinois", server = TRUE)
        updateSelectizeInput(session, 'stateSelect2', choices = fullStateName, selected = "Illinois", server = TRUE)
        updateSelectizeInput(session, 'yearSelect1', choices =  list("2000", "2010", "2018"), selected = "2000", server = TRUE)
        updateSelectizeInput(session, 'yearSelect2', choices =  list("2000", "2010", "2018"), selected = "2018", server = TRUE)
        updateSelectizeInput(session, 'checkGroup2', choices = checkboxList, selected = "All", server = TRUE)
        updateSelectizeInput(session, 'layerSelect1', choices = list("Default Mode", "B&W Mode", "Topography Mode"), selected = "Default Mode", server = TRUE)
        updateSelectizeInput(session, 'layerSelect2', choices = list("Default Mode", "B&W Mode", "Topography Mode"), selected = "Default Mode", server = TRUE)
    })
    
    output$USMap <- renderLeaflet({
        data <- NULL
        
        #Select year data corresponding to user selection
        if("2010" %in% input$yearSelect3) {
            data <- data2010
        } else if("2018" %in% input$yearSelect3) {
            data <- data2018
        } else {
            data <- data2000
        }
        
        #Terrible inefficient way to filter (done last minute)
        data <- data[data$COAL >= input$rangeSelect3[1],]
        data <- data[data$COAL <= input$rangeSelect3[2],]
        data <- data[data$OIL >= input$rangeSelect3[1],]
        data <- data[data$OIL <= input$rangeSelect3[2],]
        data <- data[data$GAS >= input$rangeSelect3[1],]
        data <- data[data$GAS <= input$rangeSelect3[2],]
        data <- data[data$NUCLEAR >= input$rangeSelect3[1],]
        data <- data[data$NUCLEAR <= input$rangeSelect3[2],]
        data <- data[data$HYDRO >= input$rangeSelect3[1],]
        data <- data[data$HYDRO <= input$rangeSelect3[2],]
        data <- data[data$BIOMASS >= input$rangeSelect3[1],]
        data <- data[data$BIOMASS <= input$rangeSelect3[2],]
        data <- data[data$WIND >= input$rangeSelect3[1],]
        data <- data[data$WIND <= input$rangeSelect3[2],]
        data <- data[data$SOLAR >= input$rangeSelect3[1],]
        data <- data[data$SOLAR <= input$rangeSelect3[2],]
        data <- data[data$GEOTHERMAL >= input$rangeSelect3[1],]
        data <- data[data$GEOTHERMAL <= input$rangeSelect3[2],]
        data <- data[data$OTHER >= input$rangeSelect3[1],]
        data <- data[data$OTHER <= input$rangeSelect3[2],]
        data <- data[data$TOTAL.RENEWABLE >= input$rangeSelect3[1],]
        data <- data[data$TOTAL.RENEWABLE <= input$rangeSelect3[2],]
        data <- data[data$TOTAL.NONRENEWABLE >= input$rangeSelect3[1],]
        data <- data[data$TOTAL.NONRENEWABLE <= input$rangeSelect3[2],]

        #Create base map
        map <- leaflet(data) %>%
               addTiles() %>%
               setView(lat = 38.61, lng = -98.811, zoom = 5)

        #Inefficiently add markers for each energy source selected
        if("Coal" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                   lng = data$LON, lat = data$LAT,
                   color = "#000000",
                   radius = ~ifelse(COAL != 0, 10, 0),
                   stroke = FALSE,
                   fillOpacity = ~ifelse(COAL != 0, .5, 0),
                   popup = ~ifelse(COAL != 0, paste(data$PLANT, "<br>", " % of Coal production: ", data$PERCENT.COAL, "<br>", "Energy Generated by Coal: ", data$COAL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
                   )
        }
        if("Oil" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .001,
                color = "#e6194b",
                radius = ~ifelse(OIL != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(OIL != 0, .5, 0),
                popup = ~ifelse(OIL != 0, paste(data$PLANT, "<br>",  " % of Oil production: ", data$PERCENT.OIL, "<br>", "Energy Generated by Oil: ", data$OIL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
                )
        }
        if("Gas" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .002,
                color = "#9A6324",
                radius = ~ifelse(GAS != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(GAS != 0, .5, 0),
                popup = ~ifelse(GAS != 0, paste(data$PLANT, "<br>",  " % of Gas production: ", data$PERCENT.GAS, "<br>", "Energy Generated by Gas: ", data$GAS, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
                )
        }
        if("Nuclear" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .003,
                color = "GREEN",
                radius = ~ifelse(NUCLEAR != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(NUCLEAR != 0, .5, 0),
                popup = ~ifelse(NUCLEAR != 0, paste(data$PLANT, "<br>", " % of Nuclear production: ", data$PERCENT.OIL, "<br>", "Energy Generated by Nuclear: ", data$OIL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste("")
                )
            )
        }
        if("Hydro" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .004,
                color = "#000075",
                radius = ~ifelse(HYDRO != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(HYDRO != 0, .5, 0),
                popup = ~ifelse(HYDRO != 0, paste(data$PLANT, "<br>", " % of Hydro production: ", data$PERCENT.HYDRO, "<br>", "Energy Generated by Hydro: ", data$HYDRO, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste("")
                )
            )
        }
        if("Biomass" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                 lng = data$LON, lat = data$LAT + .005,
                 color = "LIGHTBLUE",
                 radius = ~ifelse(BIOMASS != 0, 10, 0),
                 stroke = FALSE,
                 fillOpacity = ~ifelse(BIOMASS != 0, .75, 0),
                 popup = ~ifelse(BIOMASS != 0, paste(data$PLANT, "<br>", " % of Biomass production: ", data$PERCENT.BIOMASS, "<br>", "Energy Generated by Biomass: ", data$BIOMASS, "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, " mWh", sep = ""), paste("")
                 )
            )
        }
        if("Wind" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .006,
                color = "#469990",
                radius = ~ifelse(WIND != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(WIND != 0, .5, 0),
                popup = ~ifelse(WIND != 0, paste(data$PLANT, "<br>", " % of Wind production: ", data$PERCENT.WIND,  "<br>", "Energy Generated by Wind: ", data$WIND, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste("")
                )
            )
        }
        if("Solar" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .007,
                color = "#ffe119",
                radius = ~ifelse(SOLAR != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(SOLAR != 0, .75, 0),
                popup = ~ifelse(SOLAR != 0, paste(data$PLANT, "<br>", " % of Solar production: ", data$PERCENT.SOLAR, "<br>", "Energy Generated by Solar: ", data$SOLAR, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
            )
        }
        if("Geothermal" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .008,
                color = "PINK",
                radius = ~ifelse(GEOTHERMAL != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(GEOTHERMAL != 0, .5, 0),
                popup = ~ifelse(GEOTHERMAL != 0, paste(data$PLANT, "<br>", " % of Geothermal production: ", data$PERCENT.GEOTHERMAL, "<br>", "Energy Generated by Geothermal: ", data$GEOTHERMAL, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
            )
        }
        if("Other" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .009,
                color = "PURPLE",
                radius = ~ifelse(OTHER != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(OTHER != 0, .5, 0),
                popup = ~ifelse(OTHER != 0, paste(data$PLANT, "<br>", " % of Other production: ", data$PERCENT.OTHER, "<br>", "Energy Generated by Other sources: ", data$OTHER, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
            )
        }
        if("Renewables" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .01,
                color = "MAGENTA",
                radius = ~ifelse(TOTAL.RENEWABLE != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(TOTAL.RENEWABLE != 0, .5, 0),
                popup = ~ifelse(TOTAL.RENEWABLE != 0, paste(data$PLANT, "<br>", "Renewable Energy Generated: ", data$TOTAL.RENEWABLE, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, sep = ""), paste(""))
            )
        }
        if("Nonrenewables" %in% input$checkGroup3 || "All" %in% input$checkGroup3) {
            map <- map %>% addCircleMarkers(
                lng = data$LON, lat = data$LAT + .011,
                color = "ORANGE",
                radius = ~ifelse(TOTAL.NONRENEWABLE != 0, 10, 0),
                stroke = FALSE,
                fillOpacity = ~ifelse(TOTAL.NONRENEWABLE != 0, .5, 0),
                popup = ~ifelse(TOTAL.NONRENEWABLE != 0, paste(data$PLANT, "<br>", "Non-Renewable Energy Generated: ", data$TOTAL.NONRENEWABLE, " mWh", "<br>", "Total Energy Generated: ", data$TOTAL.GENERATION, " mWh", "<br>", "% of Renewable production: ", data$PERCENT.RENEWABLE, "<br>", "% of Non-Renewable production: ", data$PERCENT.NONRENEWABLE, " mWh", sep = ""), paste(""))
            )
        }
        
        #Add values to legend depending on check boxes selected
         if("All" %in% input$checkGroup3) {
             map %>%
             addLegend("bottomright", pal = pal, values = c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Biomass", "Wind", "Solar", "Geothermal", "Other", "Renewable", "Non-Renewable"), title = "Energy Sources")
         } else {
             map %>%
             addLegend("bottomright", pal = pal, values = input$checkGroup3, label = input$checkGroup3, title = "Energy Sources")
         }
    })
    
    #Used to reset settings on page 3, US Leaflet Map
    observeEvent(input$reset_button3, {
        updateSelectizeInput(session, 'yearSelect3', choices =  list("2000", "2010", "2018"), selected = "2018", server = TRUE)
        updateSelectizeInput(session, 'checkGroup3', choices = checkboxList, server = TRUE)
        leafletProxy("USMap") %>% setView(lat = 38.61, lng = -98.811, zoom = 5)
    })
}



shinyApp(ui = ui, server = server)
