
# Setup
library(googlesheets)
library(dplyr)
library(lubridate)
library(zoo)
library(randomForest)
library(timeDate)
library(rvest)
library(tidyr)
library(jsonlite)
library(shiny)
library(ggplot2)
library(data.table)

# Helper Functions
get_monthly_weather <- function(airport="PDX", date=as.Date("2016-12-19")) {
  
  url <- paste0('https://www.wunderground.com/history/airport/',airport,'/',
                year(date),'/',
                month(date),'/',
                day(date),'/',
                'MonthlyHistory.html')
  
  page <- read_html(url)
  closeAllConnections()
  
  weather_data <- page %>%
    html_nodes("table") %>%
    .[[4]] %>%
    html_table() %>%
    as.data.table()
  
  setnames(weather_data, c("day","temp_max","temp_avg","temp_min",
                           "dew_high","dew_avg","dew_low",
                           "humidity_high","humidity","humidity_low",
                           "pressure_high","pressure_avg","pressure_low",
                           "visibility_high","visibility_avg","visibility_low",
                           "wind_high","wind","wind_dir",
                           "precipitation","conditions"))
  weather_data = weather_data[-1]
  weather_data[,month:=month(date)]
  weather_data[,year:=year(date)]
  weather_data[,date:=ymd(paste(year,month,day,sep="-"))]
  
  # convert columns to numeric
  names = colnames(weather_data)
  ignore = c("conditions","date","precipitation")
  names = names[!names %in% ignore]
  dtnew <- weather_data[,(names):=lapply(.SD, as.numeric), .SDcols = names]
  
  # Sys.sleep(2)
}

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

# inventory = setDT(read.csv(file="inventory.csv"))

# # (my_sheets <- gs_ls())
# # fin2016 = gs_title("2016 Springwater Ledger")
pyro = gs_key("18eViJiRTmdNkYtZrwOpHqy-39RrhamL8EfemAvuhPXM")
# # gs_ws_ls(fin2016)
inventory = pyro %>% gs_read_csv(ws = "INVENTORY", skip=0) %>% as.data.table

# fixing the dates in the 2016 data
setnames(inventory,colnames(inventory),c("date","day","initial_inventory","par","prep_rec","prep_actual","waste","final_inventory","short_long","scale","use_expected","use_actual","temp","precip","clouds","sun","wind","humidity","holiday","event"))
inventory = inventory[-1]
inventory[,c("event","temp","precip","clouds","sun","wind","humidity","holiday"):=NULL]
inventory[,date:=mdy(date)]
inventory[,use_actual:=as.double(use_actual)]
inventory[,initial_inventory:=as.double(initial_inventory)]
inventory[,par:=as.double(par)]
inventory[,prep_rec:=as.double(prep_rec)]
inventory[,prep_actual:=as.double(prep_actual)]
inventory[,waste:=as.double(waste)]
inventory[,final_inventory:=as.double(final_inventory)]
inventory = inventory[use_actual!=0]
inventory = inventory[!is.na(use_actual)]

##############################
# Historical Weather ############
##############################

# date_range <- seq.Date(from=as.Date(min(inventory$date)), to=as.Date(max(inventory$date)), by='1 month')
# 
# # Initialize a data frame
# weather <- data.table()
# 
# # loop over months, and fetch weather data
# for(i in seq_along(date_range)) {
#   weather_data <- get_monthly_weather("PDX", as.Date(date_range[i]))
#   weather <- rbind(weather, weather_data)
#   print(i)
# }
# 
# # write.csv(weather,file="weather-09222017.csv")
# 
# # getting rid of css scripting
# weather[,conditions:=gsub("\n","",conditions)]
# weather[,conditions:=gsub("\t","",conditions)]
# 
# # filling in blank conditions based on average visibility
# weather[conditions=="",conditions:=ifelse(visibility_avg>8,"Overcast","")]
# weather[conditions=="",conditions:=
#           ifelse(visibility_avg<=8 & visibility_avg>6,"Mostly Cloudy","")]
# weather[conditions=="",conditions:=
#           ifelse(visibility_avg<=6 & visibility_avg>4,"Partly Cloudy","")]
# weather[conditions=="",conditions:=
#           ifelse(visibility_avg<=4 & visibility_avg>2,"Scattered Clouds","")]
# weather[conditions=="",conditions:=
#           ifelse(visibility_avg<=2,"Clear","Unknown")]
# 
# # correcting for trace precipitation
# weather$precipitation = as.numeric(as.character(weather$precipitation))
# weather[is.na(precipitation),precipitation:=0.001]
# weather[,snow:=ifelse(grepl("Snow",conditions) & !grepl("Rain",conditions),precipitation,0)]
# weather[,rain:=ifelse(grepl("Rain",conditions),precipitation,0)]
# 
# # spreading out conditions
# weather$conditions = as.character(weather$conditions)
# weather[,c("conditions1","conditions2","conditions3") := tstrsplit(conditions, ",", fixed=TRUE)]
# # weather[,conditions:=sample(c(conditions1,conditions2,conditions3),1)]
# weather[,conditions:=get_weather_condition(conditions1,conditions2,conditions3),by=date]

# save to CSV
# write.csv(weather, file=('weather-09222017-formatted.csv'), row.names=FALSE)

weather = setDT(read.csv(file="weather-09222017-formatted.csv"))
weather[,date:=as.Date(date)]

# reducing to the forecast variables
names = colnames(weather)
keep = c("date","temp_max","temp_min","conditions","rain","snow","humidity","wind")
names = names[!names %in% keep]
weather[,(names):=NULL]

##############################
# Forecast Data ############
##############################

wu.apikey = readLines("config.io",warn=F)
rwunderground::set_api_key(wu.apikey)

weather_data <- get_weather_forecast('PDX')
weather_data = setDT(weather_data$forecast$simpleforecast$forecastday)
forecast = data.table(date=seq(Sys.Date(),Sys.Date()+9,by='day'),
                      temp_max=weather_data$high[[1]],
                      temp_min=weather_data$low[[1]],
                      conditions=weather_data$conditions,
                      rain=weather_data$qpf_allday[[1]],
                      snow=weather_data$snow_allday[[1]],
                      humidity=weather_data$avehumidity,
                      wind=weather_data$avewind[[1]])
forecast[,date:=as.Date(date)]
forecast[,temp_max:=as.numeric(temp_max)]
forecast[,temp_min:=as.numeric(temp_min)]

# getting rid of "Chance of a "
gsub("Chance of a ","",forecast$conditions)
forecast$conditions = gsub("Chance of ","",forecast$conditions)

write.csv(forecast,file="forecast.csv", row.names = FALSE)
forecast = setDT(read.csv("forecast.csv"))
forecast[,date:=as.Date(date)]

# weather = setDT(read.csv(file="weather-09152017.csv"))
# weather[,date:=as.Date(date)]

# reducing to the forecast variables
# names = colnames(weather)
# keep = c("date","temp_max","temp_min","conditions","rain","snow","humidity","wind")
# names = names[!names %in% keep]
# weather[,(names):=NULL]
neworder = names(forecast)
setcolorder(weather,neworder)

weather = rbind(weather,forecast)
table(weather$conditions)

weather[conditions=="Clear",conditions:="Scattered Clouds"]
weather[conditions=="Hail",conditions:="Thunderstorm"]

##############################
# Preparing Features ############
##############################

# merging inventory and weather
dt = merge(inventory,weather,by="date",all.y=TRUE)

# adding seasons
dt[,season:=get_season(date)]

# adding holidays
dt[,holiday:=get_holiday(listHolidays("US"),date)]

# adding day of the week
dt[,day:=weekdays(date)]

# adding day
dt[,day:=weekdays(date)]

# adding day
dt[,month:=month(date)]

# #####################################
# # Delete this section after adding actual data
# #######################################
# 
# # adding in dummy use_actual until all the data is available
# dt[is.na(use_actual) & date<Sys.Date(),use_actual:=
#      as.double(sample(100:600,1)),by=date]
# 
# #########################################

# creating average use compared to previous 7 days, 3 days, 1 day
dt[,':=' (use7=rollapply(use_actual, width=list(-(7:1)), FUN=mean, fill="extend", na.rm=T),
          use3=rollapply(use_actual, width=list(-(3:1)), FUN=mean, fill="extend", na.rm=T),
          use1=rollapply(use_actual, width=list(-(1:1)), FUN=mean, fill="extend", na.rm=T))]

# creating use 3 and use 1 for forecast data
dt[,use3:=ifelse(is.na(use3),.SD[match(date - 7,.SD[,date]),use3],use3)]
dt[,use1:=ifelse(is.na(use1),.SD[match(date - 7,.SD[,date]),use1],use1)]

# #####################################
# # Delete this section after adding actual data
# #######################################
# 
# dt[is.na(use3),use3:=sample(dt[!is.na(use3),use3],1),by=date]
# dt[is.na(use1),use1:=sample(dt[!is.na(use1),use1],1),by=date]
# 
# # # eliminating conditions not in the training data
# # training_conditions = c("Fog","Overcast","Rain","Snow")
# # dt = dt[conditions %in% training_conditions]
# table(dt$conditions)
# # dt$conditions = as.character(dt$conditions)
# 
# #############################################

# adding average use for day of week by month
dt[,':=' (avgUse=mean(use_actual, na.rm=T),
             medUse=median(use_actual, na.rm=T),
             quart1Use=quantile(use_actual, na.rm=T)[2],
             quart3Use=quantile(use_actual, na.rm=T)[4],
             maxUse=max(use_actual, na.rm=T)),
      by=.(day,month)]

# center and scale all numerical features
dt[, ':=' (temp_maxz=scale(temp_max),
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
dt$conditions = as.character(dt$conditions)
dt$conditions = as.factor(dt$conditions)
dt$month = as.factor(dt$month)

##############################
# Predictor ############
##############################

load(file="rfuse.RData")

dt[,use_predicted:=round(predict(rfuse,dt,type="response"))]
dt[order(-date)]

# imp = importance(rfuse)
# MAE = mean(abs(dt$use_actual-dt$use_predicted),na.rm=T)
# MAE_baseline = mean(abs(dt$use_actual-dt$use_expected), na.rm=T)
# 
# R2 <- 1 - (sum((ntest$use_actual-ntest$use_predicted)^2)/sum((ntest$use_actual-mean(ntest$use_actual))^2))
# R2_baseline <- 1 - (sum((ntest$use_actual-ntest$use_expected)^2)/sum((ntest$use_actual-mean(ntest$use_actual))^2))

dt[,par:=ifelse(is.na(par),rollapply(use_predicted,
                             width=4,
                             align="right",
                             FUN=sum,
                             fill="extend",
                             na.rm=T),
                par)]

# importance(rfuse)
# MAE = mean(abs(dt$use_actual-dt$use_predicted))
# MAE_baseline = mean(abs(dt$use_actual-dt$use_expected))
#
# R2 <- 1 - (sum((dt$use_actual-dt$use_predicted)^2)/sum((dt$use_actual-mean(dt$use_actual))^2))
# R2_baseline <- 1 - (sum((dt$use_actual-dt$use_expected)^2)/sum((dt$use_actual-mean(dt$use_actual))^2))




