library(shiny)
library(shinydashboard)
shinyUI( 
 
  dashboardPage(
    dashboardHeader(title="Maneuver Recognition"),
    dashboardSidebar(fileInput('file1', 'Choose xlsx file',
                                 accept = c(".xlsx")),
                     sliderInput("slider1", label = h3("Sample_Range"), min = 1, 
                                 max = 30, value = 10),
                     sliderInput("slider2",label=h3("Sample_Start"),min=1,
                                 max=500,value = 1),
                     sliderInput("slider3",label=h3("Headingdiff"),min=1,
                                 max=7,value = 4,step =0.5),
                     sliderInput("slider4",label=h3("kappa"),min=1e-06,
                                 max=9e-06,step = 1e-06,value = 3e-6)
    ),
    dashboardBody(
     fluidRow( column(6,box(
        title = "Global_Map",
        collapsible = TRUE,
        width = "100%",
        height = 1000,
        leafletOutput("map1",height = 900)
     )),
       column(6,box(
        title = "Sample_Map",
        width = "100%",
        height = "100%",
        leafletOutput("map2",height = 710)
      ),box(valueBoxOutput("sw",width = "100%"),height="100%",width = "100%"))
     
       ))))