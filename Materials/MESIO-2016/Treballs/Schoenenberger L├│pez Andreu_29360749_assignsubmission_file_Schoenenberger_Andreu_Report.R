##### PACKAGES
if (!require(rvest)) install.packages("rvest", dep=TRUE); require(rvest)
if (!require(stringr)) install.packages("stringr", dep=TRUE); require(stringr)
if (!require(XML)) install.packages("XML", dep=TRUE); require(XML)
if (!require(RCurl)) install.packages("maps", dep=TRUE); require(RCurl)

##### FUNCTIONS
arrange_table <- function(one_table){
  columns.use <- c("date", "location", "accident", "event", "code", "deaths",
                   "injuries", "dose")
  columns.use.pat <- paste0(columns.use, collapse = "|")
  columns.select <- c("date", "location", "event", "code", "deaths",
                      "injuries", "dose")
  one_table <- one_table[, grep(columns.use.pat, colnames(one_table))]
  colnames(one_table)[grep("location|event|accident|dose", colnames(one_table))] <-
    c("location", "event", "dose")
  one_table <- one_table[, columns.select]
  return(one_table)
}

odd <- function(x) x%%2 != 0 
even <- function(x) x%%2 == 0 

########### CODING 
html <- getURL("http://www.johnstonsarchive.net/nuclear/radevents/radaccidents.html")
doc = htmlParse(html, asText=TRUE)
plain.text <- xpathSApply(doc, "//li", xmlValue)
table.codes <- plain.text[1:(min(grep("highest", plain.text)) - 1)][-1]
table.codes <- unlist(strsplit(table.codes, "\r\n"))
table.codes <- table.codes[grep("--", table.codes)]
table.codes <- unlist(strsplit(table.codes, "--"))
codes <- gsub(" ", "", table.codes[odd(1:length(table.codes))])
descr <- sub("^ ", "", table.codes[even(1:length(table.codes))])
descr <- gsub(" \\(.*", "", descr)
table.codes <- data.frame(code = codes, description = descr)


########### List of deadliest incidents
html <- getURL("http://www.johnstonsarchive.net/nuclear/radevents/radevents1.html")
tables <- readHTMLTable(html, stringsAsFactors = FALSE)
dead <- tables[[1]]
dead <- arrange_table(dead)

########### List of criticality accidents
html <- getURL("http://www.johnstonsarchive.net/nuclear/radcrit.html")
tables <- readHTMLTable(html, stringsAsFactors = FALSE)
critical <- tables[[1]]
critical$code <- "AC"
critical <- arrange_table(critical)

########### List of naval reactor accidents
html <- getURL("http://www.johnstonsarchive.net/nuclear/radevents/radevents3.html")
tables <- readHTMLTable(html, stringsAsFactors = FALSE)
naval <- tables[[1]]
naval <- arrange_table(naval)

########### List of criminal incidents
html <- getURL("http://www.johnstonsarchive.net/nuclear/radevents/radevents2.html")
tables <- readHTMLTable(html, stringsAsFactors = FALSE)
criminal <- tables[[1]]
criminal <- arrange_table(criminal)

########### List of nuclear test accidents
html <- getURL("http://www.johnstonsarchive.net/nuclear/radevents/radevents4.html")
tables <- readHTMLTable(html, stringsAsFactors = FALSE)
tests <- tables[[1]]
tests <- arrange_table(tests)

## ALL IN ONE TABLE
all.data <- rbind(criminal, critical, dead, naval,tests)
all.data <- all.data[!(all.data$date == ""),]

# Last 4 digits in date
years.in.order <- sort(unique(as.numeric(sub('.*(\\d{4}).*', '\\1', all.data$date))))
all.data$date <- factor(sub('.*(\\d{4}).*', '\\1', all.data$date))

par(font = 2, font.lab = 4, font.axis = 2, las = 1)
cols <- c("grey", "red")[(names(table(all.data$date)) >= 1947 & 
                            names(table(all.data$date)) <= 1991) + 1]  
barplot(table(all.data$date), xlab = "Year", 
        ylab = "Number of incidents",
        main = "Number of incidents per year",
        col = cols)
legend("topright", c("Cold War", "No Cold War"), 
       col = c("red", "grey"), lty = 1, lwd = 3, cex = 0.8,
       bty = "n")

# First digits in deaths
all.data$deaths2 <- gsub(",", "", all.data$deaths)
all.data$deaths2 <- as.numeric(gsub(" \\(.*", "", all.data$deaths2))

par(font = 2, font.lab = 4, font.axis = 2, las = 1)
plot(all.data$date, all.data$deaths2, main = "Number of deaths per year",
     xlab = "Year")

with(all.data[all.data$deaths2 < 10000,], 
     plot(date, deaths2, 
          type = "p", xlab = "Year",
          main = "Number of deaths per year excluding WWII"))

agg.by.year.d <- aggregate(all.data$deaths2[all.data$deaths2 < 10000],
                           by = list(all.data$date[all.data$deaths2 < 10000]), 
                           FUN = mean, na.rm = T)
agg.by.year.d$Group.1 <- as.character(agg.by.year.d$Group.1)
colnames(agg.by.year.d) <- c("date", "deaths")
plot(agg.by.year.d$date, agg.by.year.d$deaths, type = "l")

# First digits in injuries
all.data$injuries2 <- gsub(",", "", all.data$injuries)
all.data$injuries2 <- as.numeric(sub("([0-9]*).*$", "\\1", 
                                     gsub(" \\(.*", "", all.data$injuries2)))

plot(all.data$date, all.data$injuries2)
with(all.data[all.data$injuries2 < 10000,], plot(date, injuries2, type = "p"))

agg.by.year.i <- aggregate(all.data$injuries2[all.data$injuries2 < 10000],
                           by = list(all.data$date[all.data$injuries2 < 10000]), 
                           FUN = mean, na.rm = T)
agg.by.year.i$Group.1 <- as.character(agg.by.year.i$Group.1)
colnames(agg.by.year.i) <- c("date", "injuries")
plot(agg.by.year.i$date, agg.by.year.i$injuries, type = "l")

# Deaths + Injuries
agg.by.year <- merge(agg.by.year.d, agg.by.year.i, by = "date")
plot(agg.by.year$date, agg.by.year$deaths, type = "l", col = "black",
     ylim = c(0, max(agg.by.year$injuries, na.rm = T)), lwd = 2, 
     main = "Number of deaths + injured per year",
     xlab = "Year", ylab = "")
lines(agg.by.year$date, agg.by.year$injuries, type = "l", col = "red", lwd = 2)