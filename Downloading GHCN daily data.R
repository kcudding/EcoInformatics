
#packages#####
library(rnoaa)
library(dplyr)


#set working directory to save files
setwd("~/Desktop/Station data")

#first, download the list of all stations from the package data####
#provides a large list of all the stations in the database their functions will search from
stations <- ghcnd_stations() 

#find stations close to given locations####
#we'll use the same locations as last time
id<-c("uWaterloo", "Australia", "Amazon_Rainforest")
lat<-c(43.468889, -22, -3)
long<-c(-80.54, 134.7, -60)

#put them in one dataframe
locations<-data.frame(id,lat,long)

#we have to specify column names for latitude and longitude
#or you can just name your columns "latitude" and "longitude"
#OK, let's say we wanted all the stations within 10km
station_ids<-meteo_nearby_stations(locations, lat_colname = "lat", lon_colname = "long",
                                   station_data = stations, radius=10) #radius 10 looks for stations within 10km
station_ids$Amazon_Rainforest
station_ids$Australia
station_ids$uWaterloo

#what if we increase our radius to 100km?
station_ids<-meteo_nearby_stations(locations,lat_colname = "lat", lon_colname = "long",
                                   station_data = stations, radius=100)
station_ids$Amazon_Rainforest
station_ids$Australia
station_ids$uWaterloo

#what if we wanted just a few of the closest stations?
#change radius to limit, and put in now many of the closest stations you want
station_ids<-meteo_nearby_stations(locations,lat_colname = "lat", lon_colname = "long",
                                   station_data = stations, limit=3)

station_ids$Amazon_Rainforest
station_ids$Australia
station_ids$uWaterloo

#now, that gives us the closest stations, but not all stations have the data we need
#i.e. not all may have precipitation, or minimum daily temperature, etc.
#there is a way to specify what variables the stations need to have

#this will find the three closest stations that have precipitation (PRCP) and/or average temp (TAVG)
#note, as long as the station has one or the other variable it will be included. not both

station_ids<-meteo_nearby_stations(locations,lat_colname = "lat", lon_colname = "long",
                                   station_data = stations, limit=3,
                                   var=c("PRCP","TAVG"))
station_ids$Amazon_Rainforest
station_ids$Australia
station_ids$uWaterloo

#you can also specify dates of interest
#e.g. if you only cared about data from 2010-2019, for example
#but caution: this will find any station with any data between the years you set
#not necessarily stations with data for that entire time period

#however, this is still useful, as their database of station ID's goes back centuries in some cases
#so stations that are nearby might have been out of operation for decades 

#this time we'll set the limit to one station per location, and only look at average temperature
station_ids<-meteo_nearby_stations(locations,lat_colname = "lat", lon_colname = "long",
                                   station_data = stations, limit=1,
                                   var="TAVG",
                                   year_min = 2010,
                                   year_max = 2019)

station_ids$Amazon_Rainforest
station_ids$Australia
station_ids$uWaterloo

#at this point, let's save just the information we need from this
#we want station ids, and distance (to use as a covariate later maybe)

ids<-lapply(station_ids,function(l) l[[1]]) #1 for first column
ids<-as.data.frame(ids)#turn this into a dataframe
ids<-t(ids) #transpose it to fit our data better
ids

dist<-lapply(station_ids,function(l) l[[5]])
dist<-as.data.frame(dist)
dist<-t(dist)
dist

loc_names<-row.names(dist)

#save it as a dataframe
station.data<-cbind(loc_names,ids,dist)
rownames(station.data)<-NULL
colnames(station.data)<-c("Loc_names","IDs","Distance")
station.data<-as.data.frame(station.data)
station.data$Distance<-as.numeric(station.data$Distance)
station.data

#downloading station data#####
#in *THEORY* you can use the function "meteo_pull_monitors" to download the data directly (no url required)
#but I"ve found the function really buggy and it doesn't seem to work reliably
#but we can download the data direclty for ourselves

#the way the GHCN-Daily dataset sorts its station files is as follows:
#https://www.ncei.noaa.gov/data/daily-summaries/access/**STATION ID**.csv
#e.g. https://www.ncei.noaa.gov/data/daily-summaries/access/ACW00011604.csv

#do download data from the internet, we can use the "read.csv()" function
#for example

example<-read.csv("https://www.ncei.noaa.gov/data/daily-summaries/access/ACW00011604.csv")
head(example)
tail(example)

#we could copy and paste the individual urls for each of our stations
#but there is another option (especially useful if you have lots of stations)
#we'll use a for loop. for now, we're going to save each station file separately 

#for loops####

#used to cycle the same code multiple times (basically)

#for(number of times you want something done){
#the code you want repeated}

#for us, we want to repeat downloading the station data
#to do that, we need to make multiple urls

start<-"https://www.ncei.noaa.gov/data/daily-summaries/access/"
end<-".csv"


new.url<-paste0(start,station.data$IDs[3],end) #combines the parts
new.url

#so to make our for loop


for(i in 1:3){ #we have three stations
  new.url<-paste0(start,station.data$IDs[i],end)
  new.data<-read.csv(new.url)
  #the i represents the numbers it will cycle through
  #below code write the file name as the locaiton name and the station ID 
  write.csv(new.data, file = paste0("raw_",station.data$Loc_names[i],"_",station.data$IDs[i],end), row.names = FALSE)
}

#we can also clean the data within the for loop!
#say we are only interested in the year 2019
for(i in 1:3){ #we have three stations
  new.url<-paste0(start,station.data$IDs[i],end)
  new.data<-read.csv(new.url)
  #select only the variables you want
  new.data<-new.data %>% select(STATION,DATE,TAVG) #these headings are taken from the csvs
  #fix the date format
  new.data$DATE<-as.Date(new.data$DATE,format="%Y-%m-%d")
  #select just the time frame that we want
  new.data<-new.data[new.data$DATE>="2019-01-01" 
                   & new.data$DATE<="2019-12-31",]
  #temperature is in tenth of a degrees, so divide by 10 to correct it
  new.data[,"TAVG"]<-new.data[,"TAVG"]/10
  #this will write a csv for each location
  write.csv(new.data, file = paste0("clean_",station.data$Loc_names[i],"_",station.data$IDs[i],end),row.names = FALSE)
}

#we can also write all the stations into one big dataframe
#make an empty dataframe to store the info
storage<-data.frame(LOCATION=character(),
                    STATION=character(),
                    DATE=as.Date(character(),format="%Y-%m-%d"),
                    TAVG=numeric())

for(i in 1:3){
  new.url<-paste0(start,station.data$IDs[i],end)
  new.data<-read.csv(new.url)
  
  new.data<-new.data %>% select(STATION,DATE,TAVG) 

  new.data$DATE<-as.Date(new.data$DATE,format="%Y-%m-%d")
 
  new.data<-new.data[new.data$DATE>="2019-01-01" 
                     & new.data$DATE<="2019-12-31",]
  
  new.data[,"TAVG"]<-new.data[,"TAVG"]/10
 #this time, we want to add the location as a column (since they will all be together)
  loc_n<-rep(station.data$Loc_names[i],times=nrow(new.data))
  clean_data<-cbind(loc_n,new.data)
  #add the new clean data to the dataframe
  storage<-rbind(storage,clean_data)
}
write.csv(storage, file = "all_station_data.csv",row.names = FALSE)


head(storage)
tail(storage)



