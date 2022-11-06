## ----setup, include=FALSE------------------------------------------------
##knitr::opts_chunk$set(echo = TRUE)

## ----warning=FALSE, message=FALSE----------------------------------------
require(rvest)
# number of results pages
url <- 'http://www.habitaclia.com/alquiler-barcelona.htm'
parsedURL <- read_html(url)
numAnuncios <- '#js-list > section > div > aside.list-subtitle.f-right > h2 > span'
num <- parsedURL %>% 
  html_elements(numAnuncios) %>%
  html_text2()
num <- as.numeric(num)-1
# rental flats information
price <- NULL
info <- NULL
zone <- NULL
for(i in 0:num){
  # url
  if(i==0){
    url <- 'http://www.habitaclia.com/alquiler-barcelona.htm'
  }else{
    url <- paste0('http://www.habitaclia.com/alquiler-barcelona-',i,'.htm')
  }
  parsedURL <- read_html(url)
  # prices
  price.i <- parsedURL %>% 
    html_nodes("li div span") %>%
    html_text()
  price.i <- grep("\u20AC", price.i, value=T)
  # info
  info.i <- parsedURL %>% 
    html_nodes("div i") %>%
    html_text()
  # zone
  zone.i <- parsedURL %>% 
    html_nodes("li div span div") %>%
    html_text()
  # if all ads of a page include all the information
  if(length(price.i)==15&length(info.i)==15&length(zone.i)==15){
    price <- c(price, price.i)
    info <- c(info, info.i)
    zone <- c(zone, zone.i)
  }
  # if any ad of a page does not include all the information
  else{
    indx <- max(grep("Barcelona", zone.i))
    # price
    price.i <- parsedURL %>% 
      html_nodes(".opciones") %>%
      html_text()
    price.i <- price.i[grep("Av?same", price.i)]
    price.i <- substr(price.i, 5, regexpr("\u20AC", price.i))
    price <- c(price, price.i[1:indx])
    # info
    info2 <- parsedURL %>% 
      html_nodes(".datos") %>%
      html_text()
    info.i <- NULL
    for(j in 1:length(info2)){
      aux <- strsplit(info2[j], "\t\t\t\t")[[1]][6]
      info.i <- c(info.i, ifelse(grepl("-", aux), aux, NA))
    }
    info <- c(info, info.i[1:indx])
    # zone
    zone <- c(zone, zone.i[1:indx])
  }
}
head(price, 3)
head(info, 3)
head(zone, 3)

## ------------------------------------------------------------------------
price2 <- as.numeric(gsub("\\s\u20AC|\\.", "", price))
m2 <- as.numeric(gsub("\\.","",substr(info,1,regexpr("m2",info)-1)))
rooms <- as.numeric(substr(info,regexpr("[0-9]+\\shabitaci",info),regexpr("\\shabitaci",info)-1))
pricem2 <- as.numeric(gsub(",",".",substr(info,regexpr("[0-9]+,[0-9]+\\s\u0080",info),
                                          regexpr("\\s\u0080",info)-1)))
neighborhood <- gsub("\r\n", "", zone)
neighborhood <- gsub("Barcelona\\s", "", neighborhood)
bcn.data <- data.frame(neighborhood, price=price2, rooms, m2, pricem2)
head(bcn.data)

## ------------------------------------------------------------------------
url <- 'http://www.habitaclia.com/alquiler-vivienda-en-barcelona/provincia_barcelona-barcelones-area_6/buscardistrito.htm'
parsedURL <- read_html(url)
district <- parsedURL %>% 
  html_nodes(".verticalul") %>%
  html_text()
district <- strsplit(district, "\\s+[0-9]+\\.?[0-9]+(\r\n)+")[[1]]
#install.packages("stringi", dependencies=TRUE)
library(stringi)
district2 <- tolower(district)
district2 <- gsub("\\s", "_", district2)
district2 <- stri_trans_general(district2,"Latin-ASCII")
distr.nb <- NULL
for(i in 1:length(district2)){
  url <- paste0("http://www.habitaclia.com/alquiler-vivienda-en-barcelona-distrito_",district2[i],
                "/provincia_barcelona-barcelones-area_6/seldistrito.htm")
  parsedURL <- read_html(url)
  aux <- parsedURL %>% 
    html_nodes(".verticalul") %>%
    html_text()
  aux <- strsplit(aux, "\\s*[0-9]*\\.?[0-9]*(\r\n)+")[[1]]
  distr.nb <- rbind(distr.nb, data.frame(district=district[i], neighborhood=aux, stringsAsFactors=F))
}
head(distr.nb)

## ------------------------------------------------------------------------
bcn.data.final <- merge(distr.nb, bcn.data, by="neighborhood", all.y=T)
head(bcn.data.final)

## ------------------------------------------------------------------------
summary(bcn.data.final)

## ------------------------------------------------------------------------
bcn.data.final <- with(bcn.data.final, bcn.data.final[!is.na(price)&!is.na(m2)&!is.na(pricem2)&
                                                        price!=2595000&m2>10&pricem2>1,])
summary(bcn.data.final)

## ------------------------------------------------------------------------
distr.avg <- aggregate(cbind(price,pricem2) ~ district, data=bcn.data.final, mean)
(distr.avg <- distr.avg[order(distr.avg$price, decreasing=T),])
par(mfrow=c(1,2), mar=c(9,4,4,2))
barplot(distr.avg$price, names.arg=distr.avg$district, las=2, cex.names=1, 
        main="Average price by district")
distr.avg <- distr.avg[order(distr.avg$pricem2, decreasing=T),]
barplot(distr.avg$pricem2, names.arg=distr.avg$district, las=2, cex.names=1, 
        main="Average price per square\n meter by district")

## ----fig.height=6, fig.width=6-------------------------------------------
nb.pr <- aggregate(cbind(price,pricem2) ~ neighborhood + district, data=bcn.data.final, mean)
nb.pr <- nb.pr[order(nb.pr$price, decreasing=T),]
head(nb.pr)
palette(rainbow(10))
par(mar=c(10,4,4,2))
barplot(nb.pr$price, names.arg=nb.pr$neighborhood, col=factor(nb.pr$district), 
        las=2, cex.names=0.7, main="Average price by neighborhood")
legend("topright", levels(factor(nb.pr$district)),
       col=1:length(levels(factor(nb.pr$district))), pch=16)

## ----fig.height=6, fig.width=6-------------------------------------------
par(mar=c(10,4,4,2))
nb.pr <- nb.pr[order(nb.pr$pricem2, decreasing=T),]
barplot(nb.pr$pricem2, names.arg=nb.pr$neighborhood, col=factor(nb.pr$district), 
        las=2, cex.names=0.7, main="Average price per square meter by neighborhood")
legend("topright", levels(factor(nb.pr$district)),
       col=1:length(levels(factor(nb.pr$district))), pch=16, cex=0.75)

