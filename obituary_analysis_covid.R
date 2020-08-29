################################################################################
# ANALYZING OBITUARY NOTICES IN SOUTH TYORL DURING COVID-19
#
# written by Thomas.
################################################################################

# load packages for analyses

library(rvest)
library(stringr)
library(dplyr)
library(xts)
library(forecast)
library(ggplot2)
library(pbapply)

################################################################################

# FUNCTION DEVELOPMENT FOR ANALYSIS

# (1) Web Scraping of data

getPostContent <- function(url){
  read_html(url) %>%
    html_nodes(".tx_th-deseased-preview-text")%>%
    html_text()
}

getNextUrl <- function(url) {
  scraped_url <- read_html(url) %>% 
    html_node(".page-next") %>%
    html_attr("href")
  
  paste0("https://www.trauerhilfe.it", scraped_url)
}

scrapeBackApply <- function(url, n) {
  # url: starting url
  # n: how many subpages do we want to go back?
  pbsapply(1:n, function(x) {
    r <- getPostContent(url)
    # Overwrite global 'url'
    url <<- getNextUrl(url)
    r
  })
}

# (2) Data preperation and cleansing 

string_clean <- function(x) {
  
  res <- gsub("^\n{1,}", "", x) # eliminate \n at beginning
  res <- gsub("\n{1,}$", "", res) # eliminate \n at end
  res <- strsplit(res, "\n{1,}") # split spring by \n
  res <- unlist(res) # make vector
  res <- gsub('[^\x20-\x7E]', '', res) # eliminate strange symbols
  res <- gsub("^\n{1,}", '', res) # eliminate once again \n at beginning
  res <- str_trim(res, "both") # eliminate whitespace
  
  if(length(res) > 3) {
    
    res <- res[c(1,3,4)]
    
  }
  
  if(length(res) < 3) {
    
    res <- c(res, NA)
    
  }
  
  res <- t(matrix(res
                ,nrow=length(res)
                ,byrow=TRUE))
  
  res <- as.data.frame(res, stringsAsFactors=FALSE)
  
  return(res)
}

# source:
# https://stackoverflow.com/questions/43232549/function-for-next-page-rvest-scrape

################################################################################

# ACTUAL DATA DOWNLOAD AND PREPERATION PROCESS

url <-  "https://www.trauerhilfe.it/verstorbene/"

# use function
data_scraped <- scrapeBackApply(url, 300)

# clean data
final <- list()

for (i in 1:length(data_scraped)) {
  
  final[[i]] <- do.call(rbind.data.frame, lapply(data_scraped[[i]], string_clean))

}

# final dataset
finale <- do.call(rbind.data.frame, final)

# save data on local disc
write.csv2(finale, 
           "input_data/input/daily_deaths_2012_2020.csv",
           row.names = FALSE)

# load existing data if you don't want to rerun web scraping process
finale <- read.csv2("input_data/daily_deaths_2012_2020.csv",
                    stringsAsFactors = FALSE)

# make date vector (when data is loaded from local disc)
finale$V2 <- as.Date(finale$V2, "%Y-%m-%d")


# make date vector (when data is directly scraped from the internet)
finale$V2 <- as.Date(finale$V2, "%d.%m.%Y")


# aggretage by day
per_day <- finale %>%
  group_by(V2) %>%
  summarise(Anzahl = n()) %>%
  filter(!is.na(V2))

# make time series (start with xts object for nicer plot)
serie <- xts(per_day$Anzahl, order.by = per_day$V2)

################################################################################

# PLOT AND DATA ANALYSIS

##################
# plot daily data
##################

events <- xts("Erster Covid-19 Fall",
              as.Date(c("2020-02-24")))
plot(serie, lwd = 1, main = "tägliche Todesfälle Südtirol", minor.ticks = NULL,
     grid.col = NA, labels.col = "orangered", col = "lightgrey", major.ticks = "months",
     major.format="%b-%d", grid.ticks.on = "auto")
addPanel(rollmean, k=7, on=1, lwd = 2, col = "orangered")
addPanel(rollmean, k=30, on=1, lwd = 2, col = "orange")
addEventLines(events, srt=90, pos=2, col = "orangered", lwd = 2)
addLegend("topleft", on=1, 
          legend.names = c("tägliche Fälle", "7-Tage MA", "30-Tage MA"), 
          lty=c(1, 1), lwd=c(2, 2),
          col=c("lightgrey", "orangered", "orange"))


##################
# plot monthly data
##################

serie_monthly <-  apply.monthly(serie, sum)

# trim series until last full month
serie_monthly <- serie_monthly[-c(1:36, length(serie_monthly))]


events <- xts("Erster Covid-19 Fall",
              as.Date(c("2020-02-24")))
plot(serie_monthly, lwd = 1, main = "monatliche Todesfälle Südtirol", minor.ticks = NULL,
     grid.col = NA, labels.col = "orangered", col = "lightgrey", major.ticks = "months",
     major.format="%b-%d", grid.ticks.on = "auto")
addPanel(rollmean, k=12, on=1, lwd = 2, col = "orangered")
addEventLines(events, srt=90, pos=2, col = "orangered", lwd = 2)
addLegend("topleft", on=1, 
          legend.names = c("monatliche Fälle", "12-Monats MA"), 
          lty=c(1, 1), lwd=c(2, 2),
          col=c("lightgrey", "orangered"))

##################
# time series modelling
##################

# exclude March for model fitting
serie_monthly_model <- serie_monthly[-c(1:12, length(serie_monthly)-1,
                                        length(serie_monthly))]


xts.to.ts <- function(X, freq = 12L) {
  ts(as.numeric(X), 
     start = c(.indexyear(X)[1] + 1900, .indexmon(X)[1] + 1),
     freq = freq)
}

# create time series object
serie_monthly_ts <- xts.to.ts(serie_monthly_model)

# plot decomposition of series
plot(decompose(serie_monthly_ts), col = "orangered")

# fit ARIMA model
arima_model <- auto.arima(serie_monthly_ts)

# plot forecast
autoplot(forecast(arima_model, 24)) +
  theme_classic()

# plot actual vs fitted
plot(arima_model$x,col="orangered", main = "Actual vs fitted", lwd = 2)
lines(fitted(arima_model),col="steelblue", lwd = 2)
legend("topleft",legend=c("Actual Series", "fitted Series"),
       pch=15, col = c("orangered", "steelblue"), cex = 0.75) 


# calculate excess deaths
point_forecast <- forecast(arima_model, 2)

predicted <- sum(point_forecast$mean * c(1,0.4)) # 0.4 because April is not finished

# actual March + April
actual <- 336+121

# excess 
excess <- actual-predicted

################################################################################
################################################################################