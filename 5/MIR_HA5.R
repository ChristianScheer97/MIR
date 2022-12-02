rm(list=ls())

library(tidyverse)
library(jsonlite)

pull_acoustic_brains<-function(mbids)
{
  mbidvector<-na.omit(unique(mbids))
  cat("Asking Acousticbrainz for ", length(mbidvector), " unique IDs..\n\n")
  ergebnis_liste<-c()
  abidvector<-c()
  countvector<-c()
  anfang<-1
  stepsize<-25
  repeat
  {
    ende<-min(c(anfang+stepsize-1, length(mbidvector)))
    base_url<-"https://acousticbrainz.org"
    command<-"/api/v1/count?recording_ids="
    searchstring<-paste0(mbdidvector[anfang:ende], collapse=";")
    cat("Count request at Acousticbrainz [", anfang, ":", ende, "]: ", get, "\n\n")
    mbdidvector<-tracks$mbid
    get<-paste0(base_url, command, searchstring)
    response<-fromJSON(get)
    counts<-response %>% unlist
    abids<-gsub("count", "", response %>% unlist %>% names)
    countvector<-c(countcevtor, counts)
    abidvector<-c(abidvector, abids)
    anfang<-anfang+stepsize
    if (anfang>length(mbidvector)){break}
  }
  cat("Found: ", lenth(abidvector), " of ", length(mbidvector), " MBIDS\n\n")
  
  
  abidvector<-na.omit(unique(mbids))
  searchvector<-paste0(abidvector, ":", countvector-1)
  anfang<-1
  stepsize<-25
  repeat
  {
    ende<-min(c(anfang+stepsize-1, length(abidvector)))
    base_url<-"https://acousticbrainz.org"
    command<-"/api/v1/count?recording_ids="
    searchstring<-paste0(abdidvector[anfang:ende], collapse=";")
    cat("Count request at Acousticbrainz [", anfang, ":", ende, "]: ", get, "\n\n")
    mbdidvector<-tracks$mbid
    get<-paste0(base_url, command, searchstring)
    response<-fromJSON(get)
    counts<-response %>% unlist
    abids<-gsub("count", "", response %>% unlist %>% names)
    countvector<-c(countcevtor, counts)
    abidvector<-c(abidvector, abids)
    anfang<-anfang+stepsize
    if (anfang>length(mbidvector)){break}
  }
  cat("Found: ", lenth(abidvector), " of ", length(mbidvector), " MBIDS\n\n")
  return(countvector)
}




############## Main Program


filename="mb_test.rds"
tracklist<-read_rds(filename)
tracks<-head(tracklist, 10)

ergebnis<-pull_acoustic_brains(tracks$mbid)
