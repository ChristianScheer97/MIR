# Script to get genre whitelist for musicbrainz
{
  library(tidyverse)
  library(rvest)
}

# create whitelist
{ 
  whitelist<-read_html('https://musicbrainz.org/genres')%>%html_elements('bdi')%>%html_text()%>%as.data.frame()
  whitelist<-rename(whitelist,genres=.)
  saveRDS(whitelist,'./04_Musicbrainz_API/whitelist.rds')
}
