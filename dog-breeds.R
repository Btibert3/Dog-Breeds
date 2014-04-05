###############################################################################
## Use Petfinder website to research Dog breeds
## @brocktibert
## May 2013
###############################################################################

## set the directory for the project
setwd("~/Dropbox/Projects/Blog/Dog-Breeds")

## load the packages
library(RCurl)
library(stringr)
library(XML)
library(plyr)
library(ggplot2)
library(reshape2)


###############################################################################
## Scrape the links for all of the breeds
###############################################################################

## get all of the links
URL = "http://www.petfinder.com/dog-breeds?see-all=1"
doc = htmlParse(URL)
links <- unlist(doc['//@href'])
names(links) <- NULL

## find the links for the breeds
PATTERN = "http://www.petfinder.com/dog-breeds/.*"
breeds = as.vector(str_match(links, PATTERN))
breeds = m[!is.na(m)]
breeds = unique(breeds)

## quick summary of what we have
length(breeds)
head(breeds, 10)


###############################################################################
## grab each page - save contents to disk in the data directory
## prevents from hammering the servers during testing
###############################################################################

## grab each page
for (breed in breeds) {
 page = getURL(breed)
 PATTERN = "dog-breeds/.*$"
 bname = str_extract(breed, PATTERN)
 bname = str_sub(bname, 12)
 fname = paste0("data/", bname, ".rds")
 saveRDS(page, file=fname)
 cat("grabbed ", breed, "\n")
}


###############################################################################
## for each raw page, extract the data elements into a data frame
###############################################################################

## create the list of files
dogs = list.files("data/", full.names=T)

## for each file, parse the data into a data grame
ratings = data.frame(stringsAsFactors=F)
for (dog in dogs) {
 page = readRDS(dog)
 doc = htmlParse(page)
 tmp = xpathSApply(doc, '//*/span[@class="rate"]/node()')
 tmp = sapply(tmp, xmlValue)
 tmp = sapply(tmp, nchar)
 names(tmp) = NULL
 tmp2 = data.frame(t(unlist(as.list(tmp))))
 tmp2$breed = str_sub(dog, 6, nchar(dog)-4)
 ratings = rbind.fill(ratings, tmp2)
 cat("finished " , dog, "\n")
}

## rename the columns
names(ratings)[1:13]= c('energy','exercise','playful','affection',
                        'dogfriend','petfriend','strangefriend','training',
                        'watchdog','protection','grooming','cold','heat')

## save the file
saveRDS(ratings, file="breed-ratings.rds")


###############################################################################
## analyze the data
###############################################################################

## summary
summary(ratings)

## sort the data by training
arrange(ratings, desc(training))

## correlation
cormat = cor(ratings[,1:ncol(ratings)-1])

## plot the correlation matrix - http://goo.gl/u01aM
qplot(x=Var1, y=Var2, data=melt(cormat), fill=value, geom="tile")

## cluster the breeds
dist = dist(ratings[,1:ncol(ratings)-1])
breed.hclust = hclust(dist, method="ward")
plot(breed.hclust, labels=ratings$breed)
clus = cutree(breed.hclust, k=5)
ratings$cluster = clus

## what dogs are similar to German Shepherds
gs.clus = subset(ratings, 
                 subset = breed=='German-Shepherd-Dog', 
                 select = cluster)
gs.clus = as.numeric(gs.clus)
similar = subset(ratings, cluster==gs.clus)
similar

## instead of hclust, just try kmeans
km5 = kmeans(dist, 5)
ratings$kmeans5 = km5$cluster
with(ratings, table(breed, kmeans5))



