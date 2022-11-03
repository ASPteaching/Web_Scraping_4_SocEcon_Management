
######################## Assessment - Web Scraping Project ##########################
######################## Heike Deutelmoser                 ##########################
######################## 7th of June 2017                  ##########################


# This R code corresponds to the pdf file Deutelmoser_Heike_Module3_Assessment.pdf

# Loading required packages:
install.packages("twitteR")
install.packages("ROAuth")
install.packages("RCurl")
install.packages("stringr")
install.packages("Matrix")
install.packages("wordcloud")
install.packages("tm")
install.packages("SnowballC")
install.packages("RColorBrewer")
install.packages("topicmodels")
install.packages("data.table")
install.packages("sentiment")
install.packages("devtools")
library(twitteR)
library(ROAuth)
library(RCurl)
library(stringr)
library(Matrix)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(topicmodels)
library(data.table)
library(sentiment)
library(devtools)
install_github("sentiment140", "okugami79")




# Scraping data from twitter
# Preparation
# ("alternative") authenatication to twitter:
api_key = "OupHOPueCRA4nud5MBT5f5H8p"
api_secret = "wCnW1IJ8mgy9exXl5OdNCSXmN649ov41djfrOGgwBbOENXd27v"
access_token = "865253970440335360-feJZ6vLMWxSh5PMHe3A9fI8GNqcrQ6u" 
access_token_secret = "i9jX5y3B6htmtISr8sP9q3KNepC2UpZqByXSq1w2aVwgO" 

setup_twitter_oauth(api_key,api_secret,access_token,
                    access_token_secret)


# Scraping process
search.string <- "#lindyhop OR #swingdance OR "
no.of.tweets <- 1000
tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")
head(tweets)
tail(tweets)

# Cleaning the retrieved data
# filter the tweets of twitter user "torontolindyhop"
tweets<- userTimeline("torontolindyhop", n = 200)
head(tweets)
# [[1]]
# [1] "torontolindyhop: Don't forget to break out your best #TheLindyLook 4 #FrankieManning's Birthday Tomorrow! https://t.co/6WKlz6nvm1. https://t.co/rKDyDqgJgX"
# 
# [[2]]
# [1] "torontolindyhop: Tonight @utswing is hosting an OUTDOOR Swing Dance &amp; it's FREE! https://t.co/EBbkOEGWGl #swingdance #lindyhop #Toronto"
# 
# [[3]]
# [1] "torontolindyhop: TLH Newsletter May 23rd: Where 2 Swing Dance This Week, Lindystock, Frankie Manning Birthday, Track Podcast + More https://t.co/kY9GPj6arE"
# 
# [[4]]
# [1] "torontolindyhop: 'Where to Swing Dance in Toronto this Week' is now live on our blog! https://t.co/xD3uCA9IRs #lindyhop #swingdance. https://t.co/lhBXT2s1k2"
# 
# [[5]]
# [1] "torontolindyhop: Tonight: Barroom Jazz presented to you by the Penny Pressers at the Cameron House. 10pm, Tip the Band.. https://t.co/fIM5cCipEl"
# 
# [[6]]
# [1] "torontolindyhop: TODAY Head to Grossmans Tavern for the New Orleans Connection All Stars. Super Band, Awesome venue, great afternoon on dancing (4:30pm-8pm)"

n.tweet <- length(tweets)


# convert tweets to a data frame
tweets.df <- twListToDF(tweets)
head(tweets.df)
#text
# 1   Don't forget to break out your best #TheLindyLook 4 #FrankieManning's Birthday Tomorrow! https://t.co/6WKlz6nvm1. https://t.co/rKDyDqgJgX
# 2                  Tonight @utswing is hosting an OUTDOOR Swing Dance &amp; it's FREE! https://t.co/EBbkOEGWGl #swingdance #lindyhop #Toronto
# 3  TLH Newsletter May 23rd: Where 2 Swing Dance This Week, Lindystock, Frankie Manning Birthday, Track Podcast + More https://t.co/kY9GPj6arE
# 4 'Where to Swing Dance in Toronto this Week' is now live on our blog! https://t.co/xD3uCA9IRs #lindyhop #swingdance. https://t.co/lhBXT2s1k2
# 5             Tonight: Barroom Jazz presented to you by the Penny Pressers at the Cameron House. 10pm, Tip the Band.. https://t.co/fIM5cCipEl
# 6 TODAY Head to Grossmans Tavern for the New Orleans Connection All Stars. Super Band, Awesome venue, great afternoon on dancing (4:30pm-8pm)
# favorited favoriteCount replyToSN             created truncated replyToSID                 id
# 1     FALSE             1      <NA> 2017-05-25 19:10:53      TRUE       <NA> 867820284388179968
# 2     FALSE             2      <NA> 2017-05-24 16:00:40     FALSE       <NA> 867410024750084097
# 3     FALSE             2      <NA> 2017-05-23 19:01:39     FALSE       <NA> 867093183238373376
# 4     FALSE             3      <NA> 2017-05-23 15:45:20      TRUE       <NA> 867043777487175680
# 5     FALSE             0      <NA> 2017-05-21 19:01:01      TRUE       <NA> 866368250648645632
# 6     FALSE             0      <NA> 2017-05-21 15:25:07     FALSE       <NA> 866313915000647685
# replyToUID                                                    statusSource      screenName
# 1       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# 2       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# 3       <NA> <a href="http://www.mailchimp.com" rel="nofollow">MailChimp</a> torontolindyhop
# 4       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# 5       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# 6       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# retweetCount isRetweet retweeted longitude latitude
# 1            1     FALSE     FALSE        NA       NA
# 2            1     FALSE     FALSE        NA       NA
# 3            0     FALSE     FALSE        NA       NA
# 4            1     FALSE     FALSE        NA       NA
# 5            0     FALSE     FALSE        NA       NA
# 6            0     FALSE     FALSE        NA       NA

# Check
# check as an example tweet #85
tweets.df[85, c("id", "created", "screenName", "replyToSN",
                 "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
# id             created      screenName replyToSN favoriteCount retweetCount
# 85 842490820515237889 2017-03-16 21:40:38 torontolindyhop      <NA>             2            0
# longitude latitude
# 85        NA       NA
# text
# 85 Would give anything to have been able to dance at the Savoy in Harlem. Just one night, one night! #lindyhop. https://t.co/zzPmf0QRmD


# remove emojis
tweets.df$text <- sapply(tweets.df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
head(tweets.df)
# text
# 1    Don't forget to break out your best #TheLindyLook 4 #FrankieManning's Birthday Tomorrow! https://t.co/6WKlz6nvm1 https://t.co/rKDyDqgJgX
# 2                  Tonight @utswing is hosting an OUTDOOR Swing Dance &amp; it's FREE! https://t.co/EBbkOEGWGl #swingdance #lindyhop #Toronto
# 3  TLH Newsletter May 23rd: Where 2 Swing Dance This Week, Lindystock, Frankie Manning Birthday, Track Podcast + More https://t.co/kY9GPj6arE
# 4  'Where to Swing Dance in Toronto this Week' is now live on our blog! https://t.co/xD3uCA9IRs #lindyhop #swingdance https://t.co/lhBXT2s1k2
# 5              Tonight: Barroom Jazz presented to you by the Penny Pressers at the Cameron House. 10pm, Tip the Band. https://t.co/fIM5cCipEl
# 6 TODAY Head to Grossmans Tavern for the New Orleans Connection All Stars. Super Band, Awesome venue, great afternoon on dancing (4:30pm-8pm)
# favorited favoriteCount replyToSN             created truncated replyToSID                 id
# 1     FALSE             1      <NA> 2017-05-25 19:10:53      TRUE       <NA> 867820284388179968
# 2     FALSE             2      <NA> 2017-05-24 16:00:40     FALSE       <NA> 867410024750084097
# 3     FALSE             2      <NA> 2017-05-23 19:01:39     FALSE       <NA> 867093183238373376
# 4     FALSE             3      <NA> 2017-05-23 15:45:20      TRUE       <NA> 867043777487175680
# 5     FALSE             0      <NA> 2017-05-21 19:01:01      TRUE       <NA> 866368250648645632
# 6     FALSE             0      <NA> 2017-05-21 15:25:07     FALSE       <NA> 866313915000647685
# replyToUID                                                    statusSource      screenName
# 1       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# 2       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# 3       <NA> <a href="http://www.mailchimp.com" rel="nofollow">MailChimp</a> torontolindyhop
# 4       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# 5       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# 6       <NA> <a href="http://www.hootsuite.com" rel="nofollow">Hootsuite</a> torontolindyhop
# retweetCount isRetweet retweeted longitude latitude
# 1            1     FALSE     FALSE        NA       NA
# 2            1     FALSE     FALSE        NA       NA
# 3            0     FALSE     FALSE        NA       NA
# 4            1     FALSE     FALSE        NA       NA
# 5            0     FALSE     FALSE        NA       NA
# 6            0     FALSE     FALSE        NA       NA


# fit for slide width
# for example for tweet #85: print tweet and make text fit for slide width
writeLines(strwrap(tweets.df$text[85], 60))
# Would give anything to have been able to dance at the Savoy
# in Harlem. Just one night, one night! #lindyhop
#   https://t.co/zzPmf0QRmD


# create corpus
myCorpus<- Corpus(VectorSource(tweets.df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)


# create a copy for stem completion
myCorpusCopy <- myCorpus
myCorpus<-myCorpusCopy
myCorpus <- tm_map(myCorpus, stemDocument) # stem words



# check an example
writeLines(strwrap(myCorpus[[125]]$content, 60))
# tonight! vintag valentin dance! music @gtaswingband &;
# lesson @lhrevolut


# Analysis of the data
# Dictionary
stemCompletion2 <- function(x,dictionary) {
  x <- unlist(strsplit(as.character(x)," "))
  print("before:")
  print(x)
  x <- x[x !=""] # remove empty strings
  x <- stemCompletion(x, dictionary = dictionary)
  print("after:")
  print(x)
  x <- paste(x, sep = " ")
  PlainTextDocument(stripWhitespace(x))
}

myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)

# [1] "before:"
# [1] "world"      "lindi"      "hop"        "day"        "&;"         "franki"     "man"        "birthday!!" "#lindyhop" 
# [10] "#swingdanc"
# [1] "after:"
# world         lindi           hop           day            &;        franki           man    birthday!! 
#   "world"            ""         "hop"         "day"          "&;"     "frankie"     "manning"  "birthday!!" 
# #lindyhop    #swingdanc 
# "#lindyhop" "#swingdance" 
# [1] "before:"
# [1] "forget"        "break"         "best"          "#thelindylook" "4"             "#frankieman"   "birthday"     
# [8] "tomorrow!"    
# [1] "after:"
# forget               break                best       #thelindylook                   4         #frankieman 
# "forget"             "break"              "best"     "#thelindylook"                 "4" "#frankiemanning's" 
# birthday           tomorrow! 
#   "birthday"         "tomorrow!" 
# [1] "before:"
# [1] "tonight"    "@utsw"      "host"       "outdoor"    "swing"      "danc"       "&;"         "free!"      "#swingdanc"
# [10] "#lindyhop"  "#toronto"  
# [1] "after:"
# tonight         @utsw          host       outdoor         swing          danc            &;         free! 
#   "tonight"    "@utswing"      "hosted"     "outdoor"       "swing"       "dance"          "&;"       "free!" 
# #swingdanc     #lindyhop      #toronto 
# "#swingdance"   "#lindyhop"    "#toronto" 
# [1] "before:"
# [1] "tlh"         "newslett"    "may"         "23rd:"       "2"           "swing"       "danc"        "week,"      
# [9] "lindystock," "franki"      "man"         "birthday,"   "track"       "podcast"     "+"          
# [1] "after:"
# tlh      newslett           may         23rd:             2         swing          danc         week, 
# "tlh"  "newsletter"         "may"       "23rd:"           "2"       "swing"       "dance"       "week," 
# lindystock,        franki           man     birthday,         track       podcast             + 
#   "lindystock,"     "frankie"     "manning"   "birthday,"       "track"     "podcast"           "'" 
# [1] "before:"
# [1] "'"          "swing"      "danc"       "toronto"    "week"       "now"        "live"       "blog!"      "#lindyhop" 
# [10] "#swingdanc"
# [1] "after:"
# '         swing          danc       toronto          week           now          live         blog! 
# "'"       "swing"       "dance"     "toronto"        "week"         "now"        "live"       "blog!" 
# #lindyhop    #swingdanc 
# "#lindyhop" "#swingdance" 
# [1] "before:"
# [1] "tonight:" "barroom"  "jazz"     "present"  "penni"    "presser"  "cameron"  "house."   "10pm,"    "tip"     
# [11] "band."   
# [1] "after:"
# tonight:     barroom        jazz     present       penni     presser     cameron      house.       10pm,         tip 
# "tonight:"   "barroom"      "jazz" "presented"          ""   "presser"   "cameron"    "house!"     "10pm,"       "tip" 
# band. 
# "band'." 
# [1] "before:"
# [1] "today"        "head"         "grossman"     "tavern"       "new"          "orlean"       "connect"      "stars."      
# [9] "super"        "band,"        "awesom"       "venue,"       "great"        "afternoon"    "danc"         "(4:30pm-8pm)"
# [1] "after:"
# today          head      grossman        tavern           new        orlean       connect        stars. 
# "today"        "head"   "grossmans"      "tavern"         "new"     "orleans"  "connection"      "stars'" 
# super         band,        awesom        venue,         great     afternoon          danc  (4:30pm-8pm) 
# "super"       "band,"     "awesome"      "venue,"       "great"   "afternoon"       "dance" "4:30pm-8pm," 
# [1] "before:"
# [1] "tonight:"     "saturday"     "night"        "#swingdanc"   "w/"           "@alexpangman" "&;"           "alleycat"    
# [9] "dovercourt."  "beg"          "lesson"       "b4"          
# [1] "after:"
# tonight:       saturday          night     #swingdanc             w/   @alexpangman             &;       alleycat 
# "tonight:"     "saturday"        "night"  "#swingdance"           "w/" "@alexpangman"           "&;"    "alleycats" 
# dovercourt.            beg         lesson             b4 
# "dovercourt."          "beg"       "lesson"           "b4" 
# [1] "before:"
# [1] "tonight!"   "toronto"    "blue"       "danc"       "host"       "social"     "#bluesdanc" "ft"         "music"     
# [10] "duke"       "ellington!" "beg"        "lesson"     "avail"     
# [1] "after:"
# tonight!       toronto          blue          danc          host        social    #bluesdanc            ft 
# "tonight!"     "toronto"       "blues"       "dance"      "hosted"      "social" "#bluesdance"          "ft" 
# music          duke    ellington!           beg        lesson         avail 
# "music"        "duke"  "ellington!"         "beg"      "lesson"       "avail" 
# [1] "before:"
# [1] "3"       "ticket"  "left"    "4"       "toronto" "celebr"  "world"   "lindi"   "hop"     "day"     "(may"    "27th)"  
# [13] "&;"      "sold"    "."       "take"    "!"      


myCorpus <- Corpus(VectorSource(myCorpus))


# Count of word frequency
wordFreq <- function(corpus, word) { 
  results <- lapply(corpus, 
                    function(x){grep(as.character(x), pattern=paste0("\\<",word))} 
    )
sum(unlist(results)) 
}

# frequency for words "outdoor" and "saturday"
n.outdoor <- wordFreq(myCorpusCopy, "outdoor")
n.saturday <- wordFreq(myCorpusCopy, "saturday")
cat(n.outdoor, n.saturday)
#3 26

# Word replacement
# define function which replaces an old word with a new word
replaceWord <- function(corpus, oldword, newword) { 
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}

# replace the old word "outdoor" with the new word "open air"
myCorpus <- replaceWord(myCorpus, "outdoor", "open air")
#replace the old word "saturday" with the new word "weekend"
myCorpus <- replaceWord(myCorpus, "saturday", "weekend")


# Document matrix
# build the term document matrix
tdm<- TermDocumentMatrix(myCorpus,
                      control = list(wordLengths = c(1, Inf)))
tdm
# <<TermDocumentMatrix (terms: 572, documents: 199)>>
#   Non-/sparse entries: 2473/111355
# Sparsity           : 98%
# Maximal term length: 15
# Weighting          : term frequency (tf)

# Check
# Check the word "saturday"
idx <- which(dimnames(tdm)$Terms %in% c("saturday"))
idx
# [1] 20
as.matrix(tdm[idx, 1:200])
# Docs
# Terms      21 22 23 24 25 26 27 28 29 30
# saturday  1  0  0  0  0  0  1  0  0  0


# Word frequency
freq.terms <- findFreqTerms(tdm, lowfreq = 20)
freq.terms
# [1] "lindyhop"   "swingdanc"  "4"          "danc"       "host"       "saturday"   "swing"      "tonight"    "toronto"   
# [10] "newslett"   "tlh"        "week"       "blog"       "live"       "now"        "band"       "jazz"       "30pm"      
# [19] "awesom"     "great"      "grossman"   "head"       "new"        "today"      "beg"        "dovercourt" "lesson"    
# [28] "night"      "w"          "social" 

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)

# plot the words with its frequency
library(ggplot2) 
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))


# Word cloud
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency 
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# generate word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)


# Word associations
# associations with the word "tonight" 
findAssocs(tdm, "tonight", 0.2)
# $tonight
# lesson            beg           host             b4            9pm         social              7     dovercourt 
# 0.42           0.39           0.34           0.34           0.31           0.29           0.29           0.28 
# night        welcome    alexpangman reservoirloung           miss         semest       saturday           utsw 
# 0.28           0.25           0.24           0.24           0.24           0.24           0.23           0.23 
# w           hous          tyler           free 
# 0.23           0.22           0.22           0.21

# associations with the word "live"
findAssocs(tdm, "live", 0.2)
# $live
# blog        now    toronto       week       danc     better       glad       hear       noth swinginden         us 
# 0.80       0.76       0.51       0.40       0.35       0.34       0.34       0.34       0.34       0.34       0.31 
# support      swing       join 
# 0.27       0.23       0.20

# associations with the word "social"
findAssocs(tdm, "social", 0.2)
# $social
# beeskneesd       host      month          5    tonight        bar       cafe     steadi     beginn    everyon  bluesdanc 
# 0.43       0.35       0.32       0.30       0.29       0.29       0.29       0.29       0.26       0.24       0.23 
# utswing       duke  ellington       12am       cost  crossroad     imperi        pub        519     beauti     member 
# 0.23       0.21       0.21       0.21       0.21       0.21       0.21       0.21       0.21       0.21       0.21 
# soon       sure       11pm       miss     semest        lot    options       shoe      alway     steady    brundig 
# 0.21       0.21       0.21       0.21       0.21       0.21       0.21       0.21       0.21       0.21       0.21 
# bryan       pigg     wiggli  intermedi       mean  wednesday       page        9th        lhr 
# 0.21       0.21       0.21       0.21       0.21       0.21       0.21       0.21       0.21 


# Topic plot
dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic 
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
term
# Topic 1     Topic 2      Topic 3 Topic 4     Topic 5     Topic 6     Topic 7    Topic 8    
# [1,] "tonight"   "lesson"     "danc"  "swing"     "swing"     "swing"     "danc"     "lindyhop" 
# [2,] "swingdanc" "tonight"    "band"  "lindyhop"  "danc"      "danc"      "week"     "band"     
# [3,] "danc"      "toronto"    "week"  "week"      "lindyhop"  "lindyhop"  "toronto"  "swingdanc"
# [4,] "social"    "b4"         "live"  "swingdanc" "swingdanc" "tlh"       "lindyhop" "today"    
# [5,] "toronto"   "swingdanc"  "tlh"   "toronto"   "tavern"    "tonight"   "live"     "head"     
# [6,] "band"      "w"          "w"     "danc"      "30pm"      "swingdanc" "newslett" "music"    
# [7,] "night"     "dovercourt" "jazz"  "blog"      "connect"   "night"     "social"   "awesom" 

topics <- topics(lda) # 1st topic identified for every document (tweet) 
topics <- data.frame(date=as.IDate(tweets.df$created), topic=topics) 
ggplot(topics, aes(date, fill = term[topic])) +
geom_density(position = "stack")



# Dendrograms
# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

# plot dendrogram
plot(fit)
# cut dendrogram into 6 clusters 
rect.hclust(fit, k = 6)


# Sentiment plot
# detect sentiments
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)
# neutral positive 
# 89      110 

# define score
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "neutral"] <- 0
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
# plot sentiment plot
plot(result, type = "l")






#####################################################################################
#####################################################################################