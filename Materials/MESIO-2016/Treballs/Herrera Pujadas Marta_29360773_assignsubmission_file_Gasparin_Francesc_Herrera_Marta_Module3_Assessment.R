## PROJECTE WEB SCRAPING ##

library(XML)
library(dplyr)
library(rvest)
library(stringr)
library(rworldmap)
library(rworldxtra)
library(ggplot2)
library(maps)




ciutat <- function(vecnom){
  total <- matrix(ncol=5,nrow=length(vecnom))
  rownames(total)<-vecnom
  colnames(total) <- c("Superficie", "Población", "Densidad","CoordX", "CoordY")
  for(i in 1:length(vecnom)){
    scotusURL <- paste0("https://es.wikipedia.org/wiki/", vecnom[i])
    temp <- scotusURL %>% html %>% html_nodes("table")
  

    data_total <- as.data.frame(html_table(temp[1], fill=T))[,1:2] #llegim dades
    data_total[,1] <- unlist(lapply(data_total[,1], function(x) str_extract(pattern = "[[:alpha:]]+",x))) #llegim bé els noms
    data <- data_total[data_total[,1] %in% c("Ubicación", "Superficie", "Población", "Densidad"),] #info rellevant
  

    #Arreglem coordenades
    datacoord <-  data[1,2]
    coord <- strsplit(datacoord, "/ ")[[1]][3]
    coordX <- as.numeric(strsplit(coord,", ")[[1]][2])
    coordY <- strsplit(coord,", ")[[1]][1]
    coordY <- as.numeric(substr(coordY,2,nchar(coordY)))
  
    #Arreglem superfície
    sup <- data[data[,1]=="Superficie",2]
    sup <- gsub(" km²","", sup)
    sup <- as.numeric(gsub(",",".", sup))
  
    #Arreglem población
    pob <- data[data[,1]=="Población",2]
    pob <- gsub("[[:blank:]]hab.[[:blank:]][[:punct:]]\\d{4}[[:punct:]]", "",pob)
    pob <- as.numeric(gsub("[[:blank:]]","", pob))
  
    #Arreglem densitat
    dens <- data[data[,1]=="Densidad",2]
    dens <- gsub(" hab./km²","", dens)
    dens <- gsub(",",".",dens)
    dens <- as.numeric(gsub("[[:blank:]]","",dens))
  

    total[i,1] <- sup
    total[i,2] <- pob
    total[i,3] <- dens
    total[i,4] <- coordX
    total[i,5] <- coordY
  }
  return(as.data.frame(total))

}
noms<-c('Barcelona','Madrid','Valencia','Sevilla','Bilbao','Zaragoza',
        'Vigo','Oviedo','La_Coruña','Badajoz','Valladolid','Zamora',
        'Almeria','Ciudad_Real','Albacete','Jaen','Lérida')
dd<-ciutat(noms)

newmap <- getMap(resolution = "high")
plot(newmap, xlim = c(-10, 4), ylim = c(39, 39), asp = 1, col='grey90')
max.symbol.size=10
min.symbol.size=2
bubble.size <- (dd$Población-min(dd$Población, na.rm=T))/((max(dd$Población, na.rm=T)+.0001)-min(dd$Población, na.rm=T))*(max.symbol.size-min.symbol.size)+min.symbol.size 

points(dd[,'CoordX'], dd[,'CoordY'], pch=21, col='black',bg='darkseagreen3', cex=bubble.size)




