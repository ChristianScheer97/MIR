rm(list=ls()) # workspace

# Example Script for enriching chart data with acousticbrainz features

library(tidyverse)  # import basic functions
library(jsonlite) # import json function


pull_acousticbrainz_features<-function(mbids){
  mbidvector<-na.omit(unique(mbids))
  cat("Asking Acousticbrainz for",length(mbidvector),"unique IDs..\n\n")
  ergebnis_liste<-c()
  abidvector<-c()
  countvector<-c()
  anfang<-1
  stepsize<-25
  repeat{
    ende<-min(c(anfang+stepsize-1,length(mbidvector))) 
    base_url<-"https://acousticbrainz.org"
    command<-"/api/v1/count?recording_ids="
    searchstring<-paste0(mbidvector[anfang:ende],collapse=";")
    get<-paste0(base_url,command,searchstring)
    cat("Count request at Acousticbrainz [",anfang,":",ende,"]: ",get,"\n\n")
    response<-fromJSON(get)
    counts<-response %>% unlist
    abids<-gsub(".count","",response %>% unlist %>% names)
    countvector<-c(countvector,counts)
    abidvector<-c(abidvector,abids)
    anfang<-anfang+stepsize
    if (anfang>length(mbidvector)) {break}
  }
  cat("Found",length(abidvector),"of",length(mbidvector),"MBIDS\n\n")
  
  searchvector<-paste0(abidvector,":",countvector-1)
  anfang<-1
  stepsize<-25
  repeat{
    ende<-min(c(anfang+stepsize-1,length(abidvector))) 
    base_url<-"https://acousticbrainz.org"
    command<-"/api/v1/high-level?recording_ids="
    searchstring<-paste0(abidvector[anfang:ende],collapse=";")
    get<-paste0(base_url,command,searchstring,"&map_classes=true")
    cat("HL request at Acousticbrainz [",anfang,":",ende,"]: ",get,"\n\n")
    response<-fromJSON(get)
    
    # Auspacken fehlt 
    
    anfang<-anfang+stepsize
    if (anfang>length(mbidvector)) {break}
  }
  
  return(response)
}






############# Main Program


filename<-"mbfied_spotified_regional-global-weekly_spotify_charts_2017_2022_monthly_30.rds"

tracklist<-read_rds(filename)

tracks<-head(tracklist,5) # zum Testen mal nur die ersten 5

ergebnis<-pull_acousticbrainz_features(tracks$mb.track.id)
ergebnis

## Lösungsansatz zum "Auspacken" von HL-Infos aus dem JSON-Objekt
erg<-ergebnis %>% unlist(recursive=F)
erg2<-erg[[1]][["highlevel"]] %>% sapply("[[","value") %>% t %>% as_tibble


