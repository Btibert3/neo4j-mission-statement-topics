###############################################################################
## Use the Prismatic Interest graph to tag admissions and fin aid pages
## use neo4j to study tag associations, andy correlation with other admissions metrics
## with what is in the mission.
##
## Brock Tibert
## Feb 9, 2015
###############################################################################


## load the packages we need
library(devtools)
library(httr)
library(rvest)
library(stringr)
library(plyr)

## source the super-alpha prismatic api function
source("R/prezAPI.R")
TOKEN = "MTQyMzQwOTM4MzUxOA.cHJvZA.YnRpYmVydDNAZ21haWwuY29t.Vz2n7Zd1ks8UZ4j9fDpU3MWVogk"
# prizAPI(TOKEN, "http://www.bentley.edu/about/mission-vision-and-values")



###############################################################################
## get the ipeds data to filter our dataset to our key schools of interest
###############################################################################

## BASE URL ENDPOINT
EP = "http://nces.ed.gov/ipeds/datacenter/data/"

## download the file into our current directory
HD = paste0(EP, "HD2013.zip")
download.file(HD, destfile = "data/hd2013.zip")
IC = paste0(EP, "IC2013.zip")
download.file(IC, destfile = "data/ic2013.zip")

## unzip it into a csv in the current directory
unzip("data/hd2013.zip", exdir = "data")
unzip("data/ic2013.zip", exdir = "data")

## read in the datasets
hd = read.table("data/hd2013.csv", sep=",", header=T, stringsAsFactors=F)
colnames(hd) = tolower(colnames(hd))
ic = read.table("data/ic2013.csv", sep=",", header=T, stringsAsFactors=F)
colnames(ic) = tolower(colnames(ic))

## merge the data
ipeds = merge(hd, ic, by.x="unitid", by.y="unitid")
rm(hd, ic, HD, IC, EP)

## clean and compute some key metrics
ipeds$applcn = as.numeric(ipeds$applcn)
ipeds$admssn = as.numeric(ipeds$admssn)
ipeds$enrlt = as.numeric(ipeds$enrlt)
ipeds = transform(ipeds, admit_rate = admssn / applcn, apps_per_enroll = applcn / enrlt)


## filter the dataset
# ipeds = subset(ipeds, admit_rate < .5 & obereg %in% c(1:8) & sector %in% c(1,2) & pset4flg == 1 & enrlt > 300)
ipeds = subset(ipeds, obereg %in% c(1:8) & sector %in% c(1,2) & pset4flg == 1 & enrlt > 300)


## save the set of schools 
saveRDS(ipeds, file="output/ipeds.rds")

###############################################################################
## because we dont have the mission statements in data, crawl the 
## college navigator page and get it, if it exists
###############################################################################

## define the list of UNITIDS
UNITIDS = unique(ipeds$unitid)

## the container
mission_urls = data.frame(stringsAsFactors=F)

## comment when prod -- random sample of 5 unitids for testing
# UNITIDS = UNITIDS[sample(1:length(UNITIDS), 5)]

## for each school, grab the data
for (UNITID in UNITIDS) {
  cat("starting ", UNITID, "\n")
  ## the page
  NAV = "http://nces.ed.gov/collegenavigator/?id="
  ## build the URL
  URL = paste0(NAV, UNITID)
  message(URL)
  ## request the page
  school = html(URL)
  ## mission url
  XPATH = '//*[@id="divctl00_cphCollegeNavBody_ucInstitutionMain_dtpGeneral"]/div[1]/div/a'
  mission_url = html_nodes(school, xpath=XPATH) %>% html_text()
  ## test to ensure a url
  if (length(mission_url) == 0) {
    mission_urls = rbind.fill(mission_urls, data.frame(unitid = UNITID, url = NA))
    next
  }
  ## put into a dataframe
  df_tmp = data.frame(unitid = UNITID, url = mission_url, stringsAsFactors=F)
  mission_urls = rbind.fill(mission_urls, df_tmp)
}
rm(UNITID, NAV, URL, school, XPATH, mission_url, df_tmp)


## save the data
saveRDS(mission_urls, file="output/raw-mission-urls.rds")


###############################################################################
## get the topics of the mission statement from the prizmatic api
###############################################################################

## a prismatic container
priz = data.frame(stringsAsFactors=F)

# i in 1:nrow(mission)

## for each row with a mission statement, attempt to get data
for (i in 1:nrow(mission)) {
  cat("starting row ", i , "\n")
  ## quick slice for the row
  ROW = mission[i, ]
  ## attempt to get data
  tmp_r = tryCatch(prizAPI(TOKEN, ROW$url_clean), 
                   finally = data.frame(stringsAsFactors=F))
  #tmp_r = prizAPI(TOKEN, ROW$url_clean)
  ## append the data
  if(nrow(tmp_r) == 0) {
    tmp_r = data.frame(unitid = ROW$unitid)
  } else {
    tmp_r$unitid = ROW$unitid
  }
  ## bind to the master dataframe
  priz = rbind.fill(priz, tmp_r)
  ## pause to be nice to the API
  ## alpha limit is 20/second -- so going beyond that
  Sys.sleep(1)
}
rm(ROW, tmp_r)

## testing
# subset(priz, unitid =='164739')
# subset(priz, unitid =='164988')
# subset(priz, unitid == '164580')
## babson had not enough text -- not passing that data to the dataset, but should


## save the progress
saveRDS(priz, file="output/priz-api-results.rds")




###############################################################################
## copy the higher ed competitive graph
###############################################################################

## copy the crawled data
file.copy("~/Dropbox/Datasets/HigherEd/Cappex-April2014/parsed-data.Rdata",
          to = "data/sna.rdata")

## bring it in (meta and competitors)
load("data/sna.rdata")

## clean competitor name
from = competitors
from$comp2 = gsub("/colleges/", "", from$comp)
from$comp2 = gsub("-", " ", from$comp2)
from$comp2 = gsub("/", "", from$comp2)
from = subset(from, select =c(unitid, rank, comp2))
names(from) = c("from_unitid", "from_rank", "to_school")

## data on the "to" schools
to = meta
to = subset(to, select = c(school, unitid, rating))
names(to) = c("to_school", "to_unitid", "to_rating")

## merge the data
edges = merge(from, to, all.x=T)
edges = merge(edges, meta[,c("unitid", "rating")], by.x = "from_unitid", by.y="unitid", all.x=T )
edges = edges[, c("from_unitid", "to_unitid", "from_rank", "rating", "to_rating")]
edges = unique(edges) ## not sure why we have to do this, but its safe for analysis
names(edges)[4] = "from_rating"
edges$from_unitid = as.integer(edges$from_unitid)
edges$to_unitid = as.integer(edges$to_unitid)

## save the datasets
saveRDS(edges, file="output/comp-edges.rds")
