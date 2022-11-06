
library(twitteR)
library(tm)
library(wordcloud)
library("RColorBrewer")

###### credenciales ######
consumer_key <- "gryb8NUTvfcmnDFcTOJB2V1ao"
consumer_secret <-"ZvcbP46GwVdRVh4VTtuhjEcZ5gNGwvDB8YYuC2fVOSRCbgMRra"
access_token <-"831955046-SsmIHAyMGwm21jh7b7r0JmUPatWL8W0VzmYF5lqK"
access_secret <-"dr128iU79NWv6X15Qx8bFVTmCebwwFVnV5nZi8nLQA1qy"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

######### seleccionamos los twets de los temas ###############
mach_tweets = searchTwitter("jpmorgan", n=10000, lang="en")
mach_text = sapply(mach_tweets, function(x) x$getText())

##### inicio limpieza de datos #####
# remuevo retweets,@, simbolos de puntuación, números, links
txtclean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", mach_text)
txtclean = gsub("@\\w+", "", txtclean)
txtclean = gsub("[[:punct:]]", "", txtclean)
txtclean = gsub("[[:digit:]]", "", txtclean)
txtclean = gsub("http\\w+", "", txtclean)



# creamos el corpus  la matriz de documento:
mach_corpus = Corpus(VectorSource(txtclean))
mytwittersearch_corpus<- tm_map(mach_corpus, content_transformer(function(x)    
  iconv(enc2utf8(x), sub = "bytes")))
tdm = TermDocumentMatrix(mytwittersearch_corpus,
                         control = list(removePunctuation = TRUE,
                        stopwords = c("economy", "enough", stopwords("english")),
                          removeNumbers = TRUE, tolower = TRUE))

# Defino tdm como matriz/lo ordeno de forma decreciente de mayor a menor y lo hago un data frame.
m = as.matrix(tdm)
word_freqs = sort(rowSums(m), decreasing=TRUE) 
dm = data.frame(word=names(word_freqs), freq=word_freqs)


# exportamos el data frame para realizar la ultima limpieza
#write.table(dm, "dm.financial.csv", sep=",")
#write.table(dm, "dm.economy.csv", sep=",") 
#write.table(dm, "dm.jpm.csv", sep=",") 
#write.table(dm, "dm.gs.csv", sep=",") 

dm<-read.csv2("dm.jpm.csv")
dm0<-dm[dm$freq>100,]
barplot(dm0$freq, las = 2, names.arg = dm0$word,
        col ="blue", main ="Most frequent words",
        ylab = "Word frequencies")

png("Cloud.png", width=7, height=8, units="in", res=500)
dm<-na.omit(dm)
wordcloud(dm$word, dm$freq,min.freq = 40, random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

