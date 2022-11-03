
## Ángel Rosado y Mónica Salgado

if (!require(rvest)) install.packages("rvest", dep=TRUE)
library(rvest)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)

## Scrapping
url <- c('http://www.chistescortosbuenos.com/chistes-de-jaimito-11',
        'http://www.chistescortosbuenos.com/chistes-de.php?cat=11&page=2',
        'http://www.chistescortosbuenos.com/chistes-de.php?cat=11&page=3')
chistes <- c()
for(i in 1:3){
  chistes5 <- read_html(url[i]) %>% 
    html_nodes('.chiste') %>% 
    html_text()
  chistes <- c(chistes,chistes5[2:16])
}
#Limpiar chistes

chistes2 <- c()

for(i in 1:length(chistes)){
  text1 <- chistes[i]
  my_pattern <- "201*[0-9]"
  string_pos1 <- str_locate(text1, my_pattern)
  my_pattern2 <- "Compartir"
  string_pos2 <- str_locate(text1, my_pattern2)
  chistes2[i] <- substr(text1,start = string_pos1+4,string_pos2-1)
}
chistes2


## Todos al corpus

chistes.corpus <- Corpus(VectorSource(chistes2))

#Eliminam els espais blancs
chistes.corpus <- tm_map(chistes.corpus, stripWhitespace)

chistes.corpus <- tm_map(chistes.corpus, content_transformer(tolower))

#Canviam les lletres majúscules per minúscules.
codigos <- "\u0080|\u0085|\u0093|\u0094|\u0096|\u0096|\u0097"
chistes.corpus<- tm_map(chistes.corpus, content_transformer(gsub),
                   pattern = codigos, replacement = "")

#Eliminam els signes de puntuació.
replacePunctuation <- content_transformer(function(x) 
  {return (gsub("[[:punct:]]"," ", x))})
chistes.corpus <- tm_map(chistes.corpus, replacePunctuation )


excluidas <- c(stopwords("spanish"), "pues", "dice", "cómo", "tan",
               "mientras", "niño", "niña", "niños", "niñas")
chistes.corpus <- tm_map(chistes.corpus, removeWords, excluidas)

accents = c("á","é","í","ó","ú")
sense_accents = c("a","e","i","o","u")
for (i in 1:5){
  chistes.corpus <- tm_map(chistes.corpus, content_transformer(gsub),
                     pattern = accents[i], replacement = sense_accents[i])
}


#Ara feim un núvol de paraules amb les paraules que apareixen.

filtrado <- function(corpus, dict=NULL) {
  if (is.null(dict))
    TermDocumentMatrix(corpus)
  else
    TermDocumentMatrix(corpus, control = list(dictionary = dict))
}

p11 <- filtrado(chistes.corpus)
veces <- 1
mChistes1 <- as.matrix(p11)
frecuencias1 <- sort(rowSums(mChistes1), decreasing = TRUE)
pal21 <- brewer.pal(8,"Dark2")
set.seed(21746)
wordcloud(words=names(frecuencias1), freq=frecuencias1, scale = c(4, 0.1), min.freq=veces,
          random.order=F, colors=pal21)

