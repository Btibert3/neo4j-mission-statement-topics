###############################################################################
## Use the Prismatic Interest graph to tag admissions and fin aid pages
## use neo4j to study tag associations, andy correlation with other admissions metrics
## with what is in the mission.
##
## Brock Tibert
## Feb 9, 2015
###############################################################################


## load the packages we need
library(RNeo4j)
library(devtools)
library(httr)
library(rvest)
library(stringr)
library(plyr)



###############################################################################
## clean mission urls data
###############################################################################

## the misson urls data
ipeds = readRDS("output/ipeds.rds")
mission = readRDS("output/raw-mission-urls.rds")
priz = readRDS("output/priz-api-results.rds")
edges = readRDS("output/comp-edges.rds")


###############################################################################
## clean mission urls data
###############################################################################

## filter the data we got back for valid urls
mission = subset(mission, !is.na(url))
mission = transform(mission, url_clean = paste0("http://", url))
mission$url_clean = as.character(mission$url_clean)


###############################################################################
## clean and assemble the datasets
###############################################################################

## only include schools with topics associated with their mission statement
priz = subset(priz, !is.na(topic_id))

## keep the ipeds data for just the schools
schools = subset(ipeds, 
                 subset = unitid %in% priz$unitid,
                 select = c(unitid, instnm, stabbr, obereg, carnegie, satpct, 
                            actpct, satvr25, satvr75, satmt25, satmt75, applfeeu, ft_ug,
                            enrlt, applcn, admssn, admit_rate, apps_per_enroll))

## fix the school datat variables for whatever reason -- longhand
schools$satpct = as.numeric(schools$satpct)
schools$actpct = as.numeric(schools$actpct)
schools$satvr25 = as.numeric(schools$satvr25)
schools$satvr75 = as.numeric(schools$satvr75)
schools$satmt25 = as.numeric(schools$satmt25)
schools$satmt75 = as.numeric(schools$satmt75)
schools$applfeeu = as.numeric(schools$applfeeu)
schools$ft_ug = as.numeric(schools$ft_ug)
schools$enrlt = as.numeric(schools$enrlt)
schools$applcn = as.numeric(schools$applcn)
schools$admssn = as.numeric(schools$admssn)
schools$admit_rate = as.numeric(schools$admit_rate)
schools$apps_per_enroll = as.numeric(schools$apps_per_enroll)
schools = transform(schools, 
                    sat25 = satvr25 + satmt25,
                    sat75 = satvr75 + satmt75)
schools = transform(schools, iqr = sat75 - sat25)


## keep only the schools with complete observations
schools = schools[complete.cases(schools), ]

## keep only the topics from these schools
topics = subset(priz, unitid %in% schools$unitid)

## just for sanity, test to make sure the unitid is in both
length(unique(schools$unitid)) == length(unique(topics$unitid))

## keep only the edges from these schools
edges = subset(edges, from_unitid %in% schools$unitid & to_unitid %in% schools$unitid)

## sanity check -- some schools wont have competitor edges
length(unique(schools$unitid))
length(unique(edges$from_unitid))
length(unique(edges$to_unitid))

## save out the datasets that we will put into neo4j
save(edges, schools, topics, file="output/master-datasets.rdata")


######################################################################
## NEO4j: put the mission statement and school data into the database
######################################################################

## create a dataframe that merges all of the data together
dat = merge(topics, schools)

## put the data into neo4j
graph = startGraph("http://localhost:7474/db/data/")
graph$version

# sure that the graph is clean -- you should backup first!!!
clear(graph, input = FALSE)

## ensure the constraints
addConstraint(graph, "School", "unitid")
addConstraint(graph, "Topic", "topic_id")

## create the query
## BE CAREFUL OF WHITESPACE between KEY:VALUE pairs for parameters!!!
query = "
MERGE (s:School {unitid:{unitid},
                 instnm:{instnm},
                 obereg:{obereg},
                 carnegie:{carnegie},
                 applefeeu:{applfeeu},
                 enrlft:{enrlft},
                 applcn:{applcn},
                 admssn:{admssn},
                 admit_rate:{admit_rate},
                 ape:{ape},
                 sat25:{sat25},
                 sat75:{sat75},
                 iqr:{iqr} }) 

MERGE (t:Topic {topic_id:{topic_id},
                topic:{topic} })

MERGE (s)-[r:HAS_TOPIC]->(t)
SET r.score = {score}
"

## put the dataset in -- transactional is/should be better for larger data
start = Sys.time()
for (i in 1:nrow(dat)) {
  ## status
  cat("starting row ", i, "\n")
  ## run the query
  tryCatch(cypher(graph, 
           query, 
           unitid = dat$unitid[i],
           instnm = dat$instnm[i],
           obereg = dat$obereg[i],
           carnegie = dat$carnegie[i],
           applfeeu = dat$applfeeu[i],
           enrlft = dat$enrlt[i],
           applcn = dat$applcn[i],
           admssn = dat$admssn[i],
           admit_rate = dat$admit_rate[i],
           ape = dat$apps_per_enroll[i],
           sat25 = dat$sat25[i],
           sat75 = dat$sat75[i],
           iqr = dat$iqr[i],
           topic_id = dat$topic_id[i],
           topic = dat$topic[i],
           score = dat$score[i] ),
           finally = next)
} #endfor
end = Sys.time()
end - start


## sanity checks
(num_schools = length(unique(dat$unitid)))
(num_topics = length(unique(dat$topic_id)))
num_schools + num_topics
rm(num_schools, num_topics)

## what do we have
cypher(graph, "MATCH (n) RETURN COUNT(n)")
cypher(graph, "MATCH (n:School) RETURN COUNT(DISTINCT n)")
cypher(graph, "MATCH (n:Topic) RETURN COUNT(DISTINCT n)")



######################################################################
## NEO4j: put the edges into the database
######################################################################

## create the query
query = "
MERGE (s1:School {unitid:{from_unitid}})
SET s1.rating = {from_rating}

MERGE (s2:School {unitid:{to_unitid}})
SET s2.rating = {to_rating}

MERGE (s1) -[r:SIMILAR_TO]-> (s2)
SET r.rank = {from_rank}
"

## put the dataset in -- transactional is/should be better for larger data
start = Sys.time()
for (i in 1:nrow(edges)) {
  ## status
  cat("starting row ", i, "\n")
  ## run the query
  tryCatch(cypher(graph, 
                  query, 
                  from_unitid = edges$from_unitid[i],
                  from_rating = edges$from_rating[i],
                  to_unitid = edges$to_unitid[i],
                  to_rating = edges$to_rating[i],
                  from_rank = edges$from_rank[i] ) ,
           finally = next)
} #endfor
end = Sys.time()
end - start


## sanity checks
nrow(edges)
cypher(graph, "MATCH ()-[r:SIMILAR_TO]->() RETURN COUNT(r)")

