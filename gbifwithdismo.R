library(maps)
library(mapdata)
library(dismo) 
library(jsonlite)

#Download data from gbif site

spoc=gbif('Salmo*', geo=TRUE, download=FALSE)
#write.csv(spoc, "~Kim/kim/Ecoinformatics/salmon.csv")

# or oread previously downloaded data from drive
spoc=read.csv("~Kim/kim/hogweed/globalhogcnv2.csv")
colnames(spoc)
nrow(spoc)

# get rid of occurences without location information
spocs=subset(spoc, !is.na(lon) & !is.na(lat))
nrow(spocs)
# find and eliminate duplicate locations
spocdups=duplicated(spocs[, c("lon", "lat")])
spocs <-spocs[!spocdups, ]
nrow(spocs)

#plot to see if it looks okay
map('world', col="gray96",fill=TRUE)
points(spocs$lon, spocs$lat, pch=1, cex=0.5, col="red")
legend("left", "gbif data", pch=1, col="red", bty="n")

#investigate the data
#by region
barplot(table(spocs$country), las=2, 
        main="# reports by country")

#closeup
map('world', regions=c('usa','canada'),col="gray96",fill=TRUE, 
    xlim=c(-170,-45), ylim=c(20,80))
map('lakes', add=TRUE, col="light blue")
points(spocs$lon, spocs$lat, pch=1, cex=0.5, col="red")
legend("left", "gbif data", pch=1, col="red", bty="n")

#by time
spdat=gsub('.{9}$', '', spocs$eventDate)
spdat=as.Date(spdat, format="%Y-%m-%d") 
spocs$spdat=spdat
hist(spocs$spdat, breaks='years',xlab='years', 
     main='hogweed year of observation',
     freq=TRUE)

#look at a shorter time frame

z <- as.numeric(format(spocs$spdat[spocs$spdat>'1980-12-31'],"%Y"))
hist(z,xlab='years', 
     main='hogweed year of observation',
     freq=TRUE)

#create a new dataset of just western
recspocs=spocs[(spocs$lon<(-45)),]

z <- as.numeric(format(recspocs$spdat,"%Y"))

hist(z,breaks=min(z, na.rm=T):2020,xlab='years', 
     main='hogweed year of observation: western',
     freq=TRUE)

westoc=recspocs[!is.na(z),]
z <- as.numeric(format(westoc$spdat,"%Y"))

#bin time of observation
breaks <- c(1960,1970,1980,1990,2000, 2010,2020)
# specify interval/bin labels
tags <- c("60s","70s", "80s", "90s", "00s", "10s")
# bucketing values into bins
group_tags <- cut(z, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE,
labels=tags)
# inspect bins
summary(group_tags)


par(mar=c(0,0,0,0))

zcol=hcl.colors(length(tags), palette = "PRGn", alpha=0.75)

map('world', regions=c('usa','canada'),col="gray99",fill=TRUE, 
    xlim=c(-170,-45), ylim=c(20,80))
map('lakes', add=TRUE, col="light blue")

points(westoc$lon, westoc$lat, pch=16, cex=1, col=zcol[group_tags])
legend(x=-160, y=45, tags, pch=16, col=zcol, bty="n", title="year of record")


