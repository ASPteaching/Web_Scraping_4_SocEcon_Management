#' @title Web Scrapping ForexFactory Calendar
#' calendar starts at: 2007-01-01
#' @return 'data_raw' with all the data
#' @return csv file: yyyy-mm-dd_FF_Calendar.csv

setwd("~/Documents/R_Projects/QuantFX/MacroAnalysis")

library("rvest")    #Scrapping data
library("dplyr")    #Manipulate data frames
library("ggplot2")  #Draw nice plots
library("xts")

# Function definition -----------------------------------------------------
extract_data_of_day <- function(i, current_day_data, day) {
  # i: Iteration number
  # current_day_data: all data of the day without filters
  # day: day in Integer format
  dates <- current_day_data[i] %>%
    html_nodes("td .date") %>%
    html_text()
  
  currency <- current_day_data[i] %>% 
    html_nodes(".currency") %>%
    html_text() %>%
    trimws() %>%
    as.character()
  
  impact <- current_day_data[i] %>%
    html_nodes(".impact span") %>%
    html_attr("class") %>%
    trimws()
  
  event_name <- current_day_data[i] %>%
    html_nodes(".event span") %>%
    html_text() %>%
    trimws()
  
  actual_value <- current_day_data[i] %>%
    html_nodes(".actual") %>%
    html_text() %>%
    gsub("[^0-9&.-]", "", .) %>%
    as.numeric()
  
  forecast_value <- current_day_data[i] %>%
    html_nodes(".forecast") %>%
    html_text() %>%
    gsub("[^0-9&.-]", "", .) %>%
    as.numeric()
  
  previous_value <- current_day_data[i] %>%
    html_nodes(".previous") %>%
    html_text() %>%
    gsub("[^0-9&.-]", "", .) %>%
    as.numeric()
  
  res <- data.frame(
    "Day" = as.Date(day),
    "Currency" = currency,
    "Impact" = ifelse(length(impact) == 0, NA, as.character(impact)),
    "Event" = event_name,
    "Actual" = actual_value,
    "Forecast" = forecast_value,
    "Previous" = previous_value
  )
  return(res)
}

get_macro_data <- function(range_dates) {
  # Download EURUSD data from Fred and extract indices to use then as
  # a model when we create our 'xts' with the data
  require(fImport)
  asset <- fredSeries("DEXUSEU", from = "2007-01-01")
  asset <- as.xts(asset)
  asset <- asset[endpoints(asset, on = 'months')]
  
  # Abreviation of the months, like it is done in ForexFactory URL
  months_abbrev <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", 
                     "sep", "oct", "nov", "dec")
  
  # Loop over all days to scrap data
  dataset <- data.frame()
  for(day in range_dates) {
    cat("Fetching ", format(as.Date(day), "%Y-%m-%d"), "\n")
    
    # Extract month, day, and year to build the URL
    day_of_month <- as.integer(format(as.Date(day),"%d"))
    number_of_month <- as.integer(format(as.Date(day),"%m"))
    number_of_month <- months_abbrev[number_of_month]
    year <- format(as.Date(day),"%Y")
    
    # Contruct calendar URL, make a call and get all the data. Then we
    # filter the data to get only that we desire.
    # URL Ex: http://www.forexfactory.com/calendar.php?day=jan1.2007
    url <- paste0("http://www.forexfactory.com/calendar.php?day=", 
                  number_of_month, day_of_month, ".", year)
    web_full <- read_html(url)
    web_selected <- web_full %>% html_nodes("table .calendar_row")
    
    # Get a list with all the data and convert to matrix
    web_filtered <- lapply(X = seq_along(web_selected), FUN = extract_data_of_day, 
                  web_selected, day)
    df_day <- do.call(rbind, web_filtered) # df with daily data
    dataset <- rbind(dataset, df_day)      # df with full data
  }
  
  # Add to our data.frame the last day of the month corresponing to each date
  #Day_month <- range_dates2[match(format(dataset$Day, '%Y-%m'), format(range_dates2, '%Y-%m'))]
  Day_month <- index(asset)[match(format(dataset$Day, '%Y-%m'), format(index(asset), '%Y-%m'))]
  dataset <- cbind(Day_month, dataset)
  
  return(dataset)
}

# Run Web Scrapping! ------------------------------------------------------

# Define start and end dates
start_date <- as.Date("2017-05-01")
end_date   <- Sys.Date() 
range_dates <- seq(from = start_date, to = end_date, by = "1 day")
#range_dates2 <- seq(from = as.Date("2006-01-01"), length = 512, by = "1 month") - 1

# Start!!
t_start <- Sys.time()
data_raw <- get_macro_data(range_dates)
Sys.time() - t_start

# Save data into file
fName <- paste0(Sys.Date(), "_FF_Calendar.csv")
write.csv(x = data_raw, file = fName, row.names = FALSE)


# Load and Filter data ----------------------------------------------------
data_raw <- read.csv("2016-03-31_FF_Calendar.csv", header=TRUE, sep = ",")

filter_data <- function(full_data, currencies=c("EUR","USD"), impact=c("high", "medium")) {
  xtsList <- list()
  for(currency in currencies) {
    # Filter by currency and select unique events
    df.Currency <- subset(full_data, Currency == currency & Impact %in% impact)
    df.Events <- unique(full_data$Event)
    
    for(event in df.Events) {
      # Select only data of that event
      #df.Data <- subset(df.Currency, Event=="Spanish Manufacturing PMI")
      df.Data <- subset(df.Currency, Event==event)
      
      # If only have 'NAs' go to next iteration
      if(sum(!is.na(df.Data$Actual)) == 0) next
      print(paste(currency, "-",event, sep=" "))
    
      df.xts <- xts(df.Data$Actual, order.by = as.POSIXct(df.Data$Day_month))
      
      # Aggregate data by month when we have more than one value
      df.xts <- apply.monthly(df.xts, FUN=mean)
      colnames(df.xts) <- paste(df.Data$Event[1], currency, sep = ".")
      #colnames(df.xts) <- gsub("m.m", "", colnames(data_filtered))
      
      # Save in a list
      xtsList[[as.integer(length(xtsList)+1)]] <- df.xts
    }
  }
  data <- do.call(merge.xts, xtsList)
  
  # Count how many elements we have in each column and get last quantile
  count <- apply(data, 2, function(x) as.numeric(length(x[!is.na(x)])) )
  count_quantile <- as.numeric(quantile(count)[3])
  
  # Select data than has more than 'count_quantile' elements and carry forward
  # NA elements and then backward
  data <- data[, count>count_quantile]
  data <- na.locf(data, na.rm = FALSE)    # Fill-Forward
  data <- na.locf(data, fromLast = TRUE)  # Fill-Backward
  return(data)
}

data_filtered <- filter_data(data_raw, c("GBP", "USD"), impact = c("high", "medium"))


# Save data into file
fName <- paste0("data/GBPUSD_", Sys.Date(), "_Filtered.csv")
write.zoo(x = data_filtered, file = fName, row.names = FALSE)


# Plot data ---------------------------------------------------------------
# Count how many times appear each event
Indicator_Count<-function(names){
  as.numeric(length(grep(names,data_raw$Event)))
}

Distribution <- lapply(names(data_filtered),Indicator_Count)
#Attribute_Names_df <- as.data.frame(Events$Event)

Distribution_df <- t(data.frame(Distribution))
Indicator_Distribution <- data.frame(names(data),Distribution_df,row.names=NULL)

ggplot(Indicator_Distribution,
       aes(x=names(data),y=Distribution_df))+
  geom_bar(fill="blue",stat="identity")+
  theme(axis.text.x = element_text(size=10,angle=90,color="black"), 
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        title=element_text(size=20))+
  labs(y="Appearances in Subsets",
       x="Indicator Name",
       title="Indicator Subset Frequency")

