library(shiny)
library(leaflet)
library(sp)
library(dplyr)
library(readxl)
library(shinydashboard)
library(data.table)
library(geosphere)
library(ggplot2)
shinyServer(function(input,output,session){
  # output$contents<-renderTable({
  #   inFile<-input$file1
  #   if (is.null(inFile))
  #     return(NULL)
  #   readxl::read_xlsx(inFile$datapath)
  # })}
  points<-reactive({
    ts<-input$slider2
    samplep<-data.frame()
    index<-data.frame()
    for (i in 1:35) {
      samplep<-rbind(samplep,dataman[[i]][which(dataman[[i]][,2]>ts&dataman[[i]][,2]<(ts+0.04)),])
      if(any(dataman[[i]][,2]>ts&dataman[[i]][,2]<(ts+0.04))){
        index<-rbind.data.frame(index,as.character(dataman[[i]][1,1]),stringsAsFactors = F)
      }}
    colnames(index)<-"id"
    samplepp<-samplep[,3:4]%>%
      SpatialPoints(CRS("+proj=utm +zone=32"))%>%
      spTransform(CRS("+proj=longlat +ellps=WGS84"))%>%
      as.data.frame()%>%
      cbind(index)%>%
      cbind(samplep[,c(2,5,6,7,8,9,10,11,12,20,21,23,26,27,28,29,30)])
    
  })
  saveData <- function(data) {
    data <- as.data.frame(data)
    if (exists("responses")) {
      responses <<- rbind(responses, data)
    } else {
      responses <<- data
    }
  }
  
  
  output$map1 <-renderLeaflet({

    cars<-makeIcon(iconUrl = "http://www.slantrange.com/wp-content/uploads/leaflet-maps-marker-icons/car.png",28,28)
    
    leaflet() %>% 
      #setView(lng = 11,lat = 52,zoom=22)%>%
      setView(lng=10.7211708846226 , lat =52.2975243064831, zoom=18) %>%
      addProviderTiles("Esri.WorldImagery",options =providerTileOptions(maxZoom=25))%>%
      addMarkers(lng= points()[,1],lat=points()[,2],icon = cars,label =paste("ObjektID:",points()[,1],points()[,2],points()[,3]),layerId = points()[,3])
     
  })
  
  observe({
    click<-input$map1_marker_click
    if(is.null(click))
      return()
    daten<-points()
    move<-as.data.frame(distm(daten[,1:2]))%>%
      cbind(daten[,1:20])
    
    
    dima<-dim(as.matrix(distm(daten[,1:2])))[1]
    #for(i in 1:dima){
    # if(!any(move$id==click$id)){
    #     return()
    # }
    move<-move%>%
        mutate(xvrel=rep(move[which(id==click$id),"xv"],dima)-move[,"xv"],yvrel=rep(move[which(id==click$id),"yv"],dima)-move[,"yv"],
               xarel=rep(move[which(id==click$id),"xa"],dima)-move[,"xa"],yarel=rep(move[which(id==click$id),"ya"],dima)-move[,"ya"])%>%
        mutate(vrel=sqrt(xvrel^2+yvrel^2))%>%
        cbind(dist1=c(t(move[which(move$id==click$id),1:dima])),stringsAsFactors = F)

    if(any(move$id==click$id)){
     jxv<-move[which(move$id==click$id),"xv"]
     jyv<-move[which(move$id==click$id),"yv"]
     move1<-move%>%
       filter((xv*jxv)>=0&(yv*jyv)>=0)
       
     if(!exists("move1")){
       return(NULL)
     }
    }

    
    
    table1<-t(move1[,"dist1"])
    if(nrow(move1)<2)
      return()
    colnames(table1)<-c(move1[,"id"])
    output$Click_text<-renderTable(
    #valueBox("Distance to each cars",text,icon = icon("car"),color = "green")
       table1
    )
    if(dima<2)
      return()
    if(!exists("move1"))
      return()
    lng1<-move1[which(move1$id==click$id),"lng"]
    lat1<-move1[which(move1$id==click$id),"lat"]
    xv1<- move1[which(move1$id==click$id),"xv"] 
    move2<-move1%>%                                                                                                         
      mutate(ann=ifelse(lng<lng1&lat<lat1&xv<xv1&dist1<50,"drive away",ifelse(lng<lng1&lat<lat1&xv>xv1&dist1<50,"approach",ifelse(lng>lng1&lat>lat1&xv<xv1&dist1<50,"approach",ifelse(lng>lng1&lat>lat1&xv>xv1&dist1<50,"drive away","unknown")))))%>%
      mutate(spur=ifelse((headingdiff>3)&(kappediff<=-1e-9),"Links Spurwechsel",ifelse((headingdiff<=-3)&(kappediff>=1e-9),"Rechts Spurwechsel","geradeaus")))
  
    
  
  
  
    output$sw<-renderValueBox({
      click<-input$map1_marker_click
      if(is.null(click)){
        valueBox(
          "wait for input","null",icon = icon("car"),color = "red")}else{
            sample<-points()
           # colnames(data2)<-c("lng","lat","id","ts","xv","yv","xa","ya","xsize","ysize","zsize","heading","vabs","aabs")
            # sample<-data2%>%
            #   mutate(kappe= (xa*yv-ya*xv)/(((xv)^2+(yv)^2)^(3/2)))
            if(!any(sample$id==click$id))
              return(valueBox(
                "wait for input","null",icon = icon("car"),color = "red"))
            if(sample[which(sample$id==click$id),"headingdiff"]>=3&sample[which(sample$id==click$id),"kappediff"]<=-1e-9){
              swo<-"Links Spurwechsel"
              col<-"green"}else if(sample[which(sample$id==click$id),"headingdiff"]<=-3&sample[which(sample$id==click$id),"kappediff"]>=1e-9){
                swo<-"Rechts Spurwechsel"
                col<-"blue" }else{
                  swo<-"Geradeaus fahren"
                  col<-"yellow"    
                }
            if(!is.character(swo)){
              valueBox(
                "wait for input","null",icon = icon("car"),color = "red")
            }else{
              valueBox(
                "What happend",swo,icon = icon("car"),color = col)
            }}
   })
    observeEvent(move2,{
      if(!exists("akk")){
        akk<<-data.frame()
      }
      
      if(exists("move2")&&!is.na(move2)){
        akk<<-rbind(akk,move2[,(ncol(move2)-25):ncol(move2)],stringsAsFactors = F)
      }
      if(!is.null(input$submit)){
      output$manue<-renderPlot({
        ggplot()+
          geom_point(data=akk,aes(x=ts,y=id,color=spur,size=4))+
          scale_color_manual(breaks=c("Links Spurwechsel","Rechts Spurwechsel","geradeaus"),values = c("Links Spurwechsel"="red","Rechts Spurwechsel"= "blue", "geradeaus"="green"))+
          xlim(1509096323,1509096383)
        })
      output$longi<-renderPlot({
        ggplot()+
          geom_point(data=akk,aes(x=ts,y=id,color=ann,size=4))+
          scale_color_manual(breaks=c("drive away","approach","unknown"),values = c("drive away"="red","approach"="blue","unknown"="green"))+
          xlim(1509096323,1509096383)
      })
      }
    })
  })



  })
 
  
