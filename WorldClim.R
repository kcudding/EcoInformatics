#R code to be demonstrated in WorldClim ecoinformatics seminar

#packages####
library(sp)
library(raster)

#set my working directory
setwd("~/Desktop/WorldClim")
#bioclimatic variables####
#bioclimatic variables are from a list of 19 (seen: https://www.worldclim.org/data/bioclim.html)

biodata<-getData("worldclim", var="bio", res=10) #downloads all 19 variables at chosen resolution

#from the list of 19, choose which variables you want
#we'll look at: Annual Mean Temperature (BIO1) & Annual Precipitation (BIO12)    

biodata<-biodata[[c(1,12)]] #numbers chosen for their position in the list
names(biodata)<-c("Annual_Temp","Annual_Precip")

#plot the data###
plot(biodata[["Annual_Temp"]], main="Annual Average Temp")
plot(biodata[["Annual_Precip"]], main="Annual Average Precipitation")

#what if you're only interested in a part of the world. Like North America
subset <- crop(biodata, extent(-170,-45,10,85)) #subsets the map by long/lat
#divide by 10 to correct for temperature
plot(subset[["Annual_Temp"]]/10, main="Annual Average Temp") 
plot(subset[["Annual_Precip"]], main="Annual Average Precipitation")

#now we need our coordinates of interest
locations<-c("uWaterloo", "Australia", "Amazon_Rainforest")
lat<-c(43.468889, -22, -3) #lat/long must be in decimal form
long<-c(-80.54, 134.7, -60)

#we need a dataframe of just the lat/long
#but the longitude must be the first column (latitude column 2)

coords<-data.frame(longitude=long, latitude=lat)

#convert the data into spatial points for our chosen locations
loc<-SpatialPoints(coords, proj4string = biodata@crs)
#CRS is a standard way of describing locations in R (Coordinate reference system)

#using this data, we can extract the values
variables<-extract(biodata,loc)
variables<-as.data.frame(variables)
head(variables)
#temperatures are shown to a tenth of a degree, so divide by 10
variables$Annual_Temp<-variables$Annual_Temp/10
head(variables)

#plot the data to make sure locations are correct
plot(biodata[["Annual_Temp"]]/10)
plot(loc,add=T)

#export the data
#combine the data with the location dataframe
loc_df<-cbind(locations,coords,variables)
loc_df

write.csv(loc_df,"locations with worldclim variables",row.names = FALSE)


#worldclim variables#####
#the process is very similar, but instead of downloading multiple variables
#it downloads one variable that you choose, and gives you monthly averages
worlddata<-getData("worldclim", var="tmin",res=10) #this time we specify we want "tmin" or minimum monthly temperature

names(worlddata)<- c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                     "Aug","Sept","Oct","Nov","Dec")

plot(worlddata[["Jan"]]/10, main="Jan. Average Minimum Temperature")
plot(worlddata[["Jul"]]/10, main="Jul. Average Minimum Temperature")

#again, if we want to subset
subset2<-crop(worlddata, extent(105,170,-50,-5))

plot(subset2[["Jan"]]/10, main="Jan. Average Minimum Temperature")
plot(subset2[["Jul"]]/10, main="Jul. Average Minimum Temperature")

#to save the variables at specific locations it is the same code as before
#with the new data subbed in

#we'll use the same coordinates
#coords<-data.frame(longitude=long, latitude=lat)
loc.min<-SpatialPoints(coords, proj4string = worlddata@crs)
variables.min<-extract(worlddata,loc.min)
variables.min<-as.data.frame(variables.min)
head(variables.min)
#again divide by 10 to fix temperatures
variables.min<-variables.min/10
head(variables.min)


#combine the data with the location dataframe
min_df<-cbind(locations,coords,variables.min)
min_df

write.csv(min_df,"minimum monthly average temperatures",row.names = FALSE)


#future projections####
#while WorldClim has CMIP6 data, the raser package currently only supports up to CMIP5

future.data<-getData("CMIP5", var="tmin", res=10, rcp=85, model="AC", year=70)
#CMIP5 calls future data, and var="tmin" calls estimatd maximum monthly temperatures
#res is still resoltuion
#rcp, model and year are unique to future data. year can be either 50 (for 2041-2061 predictions)
#or 70 for 2061-2080 predictions
#rcp and model are economic future possibilites and different model options to choose from
#here I just chose two
names(future.data)<- c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                     "Aug","Sept","Oct","Nov","Dec")
plot(future.data[["Jan"]]/10, main="Predicted Jan. Ave. Minimum Temperature")
plot(future.data[["Jul"]]/10, main="Predicted Jul. Ave. Minimum Temperature")

#we can see the difference if we compare this projection to the older data
par(mfrow=c(2,1))
plot(worlddata[["Jan"]]/10, main="Old Temp(min)")
plot(future.data[["Jan"]]/10, main="Predicted Temp(min)")
par(mfrow=c(1,1))


