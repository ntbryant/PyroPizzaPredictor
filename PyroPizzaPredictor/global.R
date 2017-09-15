
# Setup
library(googlesheets)
library(data.table)
library(dplyr)
library(plyr)
library(lubridate)
library(zoo)
library(randomForest)
library(timeDate)
library(rvest)
library(tidyr)

# Helper Functions
get_weather_forecast <- function(airport="PDX")
{
  base_url <- paste0('http://api.wunderground.com/api/',wu.apikey,'/')
  final_url <- paste0(base_url, 'forecast10day/q/',airport, '.json')
  
  # reading in as raw lines from the web service
  conn <- url(final_url)
  raw_data <- readLines(conn, n=-1L, ok=TRUE)
  # Convert to a JSON
  weather_data <- fromJSON(paste(raw_data, collapse=""))
  close(conn)
  return(weather_data)
}

get_season = function(dates) {
  WS = as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE = as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS = as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE = as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d = as.Date(strftime(dates, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

get_holiday = function(holidays=listHolidays("US"),dates) {
  
  years = year(dates)
  years_levels = levels(as.factor(years))
  
  holiday_date = data.table()
  
  for (h in holidays) {
    for (y in years_levels) {
      y = as.list(as.numeric(y))
      holiday_date = rbind(holiday_date,do.call(h,y))
    }
  }
  
  holiday_date = as.Date(holiday_date$`GMT:x`)
  
  holiday = ifelse(dates %in% holiday_date,1,0)
}

get_weather_condition = function(conditions1,conditions2,conditions3) {
  
  if (!is.na(conditions2) & !is.na(conditions3)) {
    c = sample(c(conditions1,conditions2,conditions3),1)
  } else if (!is.na(conditions2)) {
    c = sample(c(conditions1,conditions2),1)
  } else {
    c = conditions1
  }
  return(c)
}

##############################
# Pyro Pizza Data ############
##############################

# (my_sheets <- gs_ls())
# fin2016 = gs_title("2016 Springwater Ledger")
pyro = gs_key("1MzRbJdaHKv9CJMPeN7Z-WQtdoGuLWAyE9gvYS-_Mgc8")
# gs_ws_ls(fin2016)
inventory = pyro %>% gs_read_csv(ws = "12th INVENTORY", skip=0) %>% as.data.table

# fixing the dates in the 2016 data
setnames(inventory,colnames(inventory),c("date","day","initial_inventory","par","prep_rec","prep_actual","waste","final_inventory","short_long","scale","use_expected","use_actual","temp","precip","clouds","sun","wind","humidity","holiday","event"))
inventory = inventory[-1]
inventory[,c("event","temp","precip","clouds","sun","wind","humidity","holiday"):=NULL]
inventory[,date:=mdy(date)]
inventory[,use_actual:=as.double(use_actual)]
inventory = inventory[use_actual!=0]

##############################
# Forecast Data ############
##############################

wu.apikey = readLines("./../config.io",warn=F)
rwunderground::set_api_key(wu.apikey)

weather_data <- get_weather_forecast('PDX')
weather_data = setDT(weather.data$forecast$simpleforecast$forecastday)
forecast = data.table(date=seq(Sys.Date(),Sys.Date()+9,by='day'),
                      temp_max=weather.data$high[[1]],
                      temp_min=weather.data$low[[1]],
                      conditions=weather.data$conditions,
                      rain=weather.data$qpf_allday[[1]],
                      snow=weather.data$snow_allday[[1]],
                      humidity=weather.data$avehumidity,
                      wind=weather.data$avewind[[1]])
forecast[,date:=as.Date(date)]

# forecast = setDT(read.csv(file="./../weather-09152017.csv"))
# forecast[,date:=as.Date(date)]

##############################
# Preparing Features ############
##############################

# merging inventory and weather
dt = merge(inventory,forecast,by="date")

# adding seasons
dt[,season:=get_season(date)]

# adding holidays
dt[,holiday:=get_holiday(listHolidays("US"),date)]

# adding day of the week
dt[,day:=weekdays(date)]

# creating average use compared to previous 7 days, 3 days, 1 day
dt[,':=' (use7=rollapply(use_actual, width=list(-(7:1)) , FUN=mean, fill="extend"),
          use3=rollapply(use_actual, width=list(-(3:1)) , FUN=mean, fill="extend"),
          use1=rollapply(use_actual, width=list(-(1:1)) , FUN=mean, fill="extend"))]

# eliminating single condition instances
table(dt$conditions)
dt = dt[conditions!="Thunderstorm"]

# adding average use for day of week by season
dt[,':=' (avgUse=mean(use_actual),
             medUse=median(use_actual),
             quart1Use=quantile(use_actual)[2],
             quart3Use=quantile(use_actual)[4],
             maxUse=max(use_actual)),
      by=day]

# center and scale all numerical features
dt[,':=' (temp_maxz=scale(temp_max),
             temp_minz=scale(temp_min),
             humidityz=scale(humidity),
             windz=scale(wind),
             snowz=scale(snow),
             rainz=scale(rain),
             use7z=scale(use7),
             use3z=scale(use3),
             use1z=scale(use1),
             avgUsez=scale(avgUse),
             medUsez=scale(medUse),
             quart1Usez=scale(quart1Use),
             quart3Usez=scale(quart3Use),
             maxUsez=scale(maxUse))]

# making the categorical variables factors
dt$day = as.factor(dt$day)
dt$season = as.factor(dt$season)
# dt$conditions = as.character(dt$conditions)
dt$conditions = as.factor(dt$conditions)

##############################
# Predictor ############
##############################

load(file="./../rfuse.RData")

dt[,use_predicted:=round(predict(rfuse,dt,type="response"))]

importance(rfuse)
MAE = mean(abs(dt$use_actual-dt$use_predicted))
MAE_baseline = mean(abs(dt$use_actual-dt$use_expected))

R2 <- 1 - (sum((dt$use_actual-dt$use_predicted)^2)/sum((dt$use_actual-mean(dt$use_actual))^2))
R2_baseline <- 1 - (sum((dt$use_actual-dt$use_expected)^2)/sum((dt$use_actual-mean(dt$use_actual))^2))


