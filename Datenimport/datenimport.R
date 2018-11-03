options(digits = 22)
j<-0
i<-1
dataman<-list()
dataman[[i]]<-fread("raw.csv",skip = j)
while(i!=36){
     i<-i+1
     j<-j+nrow(dataman[[i-1]])+2
     dataman[[i]]<-fread("raw.csv",skip = j)
}
for(i in 1:length(dataman)){
  colnames(dataman[[i]])<- c("id","ts","lng","lat","xv","yv","xa","ya","xsize","ysize","zsize","heading","pedestrain_p","bicyclist_p","motorcyc_p","car_p","van_p","truck_p","t0","vabs","aabs","signed_a","heading_a","class","unknown")
}
for(i in 1:length(dataman)){
   dataman[[i]]<-dataman[[i]]%>%
    mutate(kappe= (xa*yv-ya*xv)/((((xv)^2)*((yv)^2))^(3/2)))%>%
    mutate(kapped=diff(c(0,kappe)))%>%
    mutate(kappedd=diff(c(0,kapped)))
}
for(i in 1:length(dataman)){
  mean<-mean(dataman[[i]]$heading)
  dataman[[i]]<-dataman[[i]]%>%
    mutate(headingdiff=diff(c(rep(mean,8),heading,rep(mean,8)),16))
}
for(i in 1:length(dataman)){
  mean<-mean(dataman[[i]]$kapped)
  dataman[[i]]<-dataman[[i]]%>%
    mutate(kappeddiff=diff(c(rep(mean,10),kapped,rep(mean,10)),20))
}
for(i in 1:length(dataman)){
  dataman[[i]]$kappeddiff<-NULL
}
for(i in 1:length(dataman)){
  mean<-mean(dataman[[i]]$kappe)
  dataman[[i]]<-dataman[[i]]%>%
    mutate(kappediff=diff(c(rep(mean,10),kappe,rep(mean,10)),20))
}