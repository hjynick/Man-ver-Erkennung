library(shiny)
library(leaflet)
library(sp)
library(dplyr)
library(readxl)
library(shinydashboard)
shinyServer(function(input,output){

  output$map2 <-renderLeaflet({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    data<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    i<-input$slider2
    j<-input$slider1
    sample<-data[i:(i+j),]%>%
      select("x","y")%>%
      SpatialPoints(CRS("+proj=utm +zone=32"))%>%
      spTransform(CRS("+proj=longlat +ellps=WGS84"))
    
    sample1<-as.data.frame(sample)
    
    leaflet() %>% 
      setView(lng=mean(sample1[,1]) , lat =mean(sample1[,2]), zoom=22)%>%
      addProviderTiles("Esri.WorldImagery", group="background 1",options =providerTileOptions(maxZoom=25) )%>%
      addTiles(options = providerTileOptions(noWrap = TRUE,maxZoom=25), group="background 2")%>%
      addProviderTiles("OpenStreetMap.BlackAndWhite",group = "background 3",options =providerTileOptions(maxZoom=25))%>%
      #addProviderTiles("OpenPtMap",group = "background 4") %>%
      #addCircleMarkers(data = sample,radius = 1,fill = FALSE,opacity = 1)%>%
      addAwesomeMarkers(data = sample)%>%
      addLayersControl(
        baseGroups = c("background 1","background 2","background 3"), 
        options = layersControlOptions(collapsed = FALSE))
 
  })
  output$map1 <- renderLeaflet({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    data<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    i<-input$slider2
    j<-input$slider1
    sample<-data[i:(i+j),]%>% 
      select("x","y")%>%
      SpatialPoints(CRS("+proj=utm +zone=32"))%>%
      spTransform(CRS("+proj=longlat +ellps=WGS84"))
    sample1<-as.data.frame(sample)
    # weit<-max(sample1)-min(sample1)
    gpsdata<-data%>%
      select("x","y")%>%
      SpatialPoints(CRS("+proj=utm +zone=32"))%>%
      spTransform(CRS("+proj=longlat +ellps=WGS84"))
    gpsdata1<-as.data.frame(gpsdata)
    
    leaflet() %>% 
      setView(lng=mean(gpsdata1[,1]) , lat =mean(gpsdata1[,2]), zoom=18) %>%
      addProviderTiles("Esri.WorldImagery", group="background 1",options =providerTileOptions(maxZoom=25) ) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE,maxZoom=25), group="background 2")%>%
      addProviderTiles("OpenStreetMap.BlackAndWhite",group = "background 3",options =providerTileOptions(maxZoom=25)) %>%
      addCircleMarkers(data = gpsdata,radius = 1,fill = TRUE,opacity = 1)%>%
      #addAwesomeMarkers(data = gpsdata,icon = awesomeIcons(icon = 'arrow-round-up',iconColor = 'black',library = 'ion'),label = as.character(gpsdata$x))%>%
      addCircles(lng=mean(sample1[,1]) , lat =mean(sample1[,2]),radius = 90000*diff(range(sample1[,2])),color = "red")%>%
      addLayersControl(
        baseGroups = c("background 1","background 2","background 3"), 
        options = layersControlOptions(collapsed = FALSE))
      
})

  output$sw<-renderValueBox({
    inFile <- input$file1
    if(is.null(inFile)){
      valueBox(
        "wait for input","null",icon = icon("car"),color = "red")}else{
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    data<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    i<-input$slider2
    j<-input$slider1
    sample<-data[i:(i+j),]%>%
      mutate(kappe= abs(x_a*y_v-y_a*x_v)/(((x_v)^2+(y_v)^2)^(3/2)))%>%
      select("heading","kappe")

    if((diff(range(sample[1:j,1]))>input$slider3)&(colSums(sample[,2],na.rm = TRUE)>j*input$slider4)){
      swo<-"Spurwechsel"
      col<-"green"} else{
        swo<-"Kein Spurwechsel"
        col<-"yellow"}
    if(!is.character(swo)){
      
       valueBox(
         "wait for input","null",icon = icon("car"),color = "red")
    }else{
     valueBox(
      "What happend",swo,icon = icon("car"),color = col
    )}}
  })
    
  })

 # })
  
