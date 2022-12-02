rm(list=ls()) # workspace

# Example Script for Scraping the global charts from Spotify

library(tidyverse)  # import basic functions
library(rvest) # import scraping functions
library(RSelenium) # import scraping for dynamic websites
library(lubridate)

spotiscrape<-function(page, number){
  number<-200
  remDr$navigate(page)   # gehe auf die Webseite "page"
  cat("Extracting data from page",page,"\n")
  Sys.sleep(2)  # warte 2 Sekunden
  seite<-read_html(remDr$getPageSource()[[1]]) # besorge Dir dynamische generierte Seite aus Browser
  
  actual_number<-length(html_elements(seite,".hgLxdb") %>% html_attr("href"))
  if(actual_number!= number)
  {
    number<-actual_number
  }

  links<-html_elements(seite,"a") %>% html_attr("href")  # ziehe dir alle href-Elemente aus den a-Elementen der Seite
  spotify_id<-links[grep('track',links)][c(T,F)] # nimm nur die Links welche "track" enthalten und nur jeden zweiten
  spotify_id<-spotify_id[seq(1:number)]
  ids<-gsub("https://open.spotify.com/track/","",spotify_id) %>% as.data.frame()  # entferne den URL-Teil und speichere es als Datenspalte
  
  position<-html_elements(seite,".hgLxdb")[seq(1:number)] %>% html_text() %>% as.integer() %>% as.data.frame() #nimm Dir Text aus allen hgLxdb-Elememten, wandle in Integer und speichere als Datenspalte
  titles<-html_elements(seite,".kKOJRc")[seq(1:number)] %>% html_text() %>% as.data.frame()
  artists<-html_elements(seite,".lfGOlT")%>% html_text()
  artists<-artists[1:number+1] %>% as.data.frame()
  streams<-html_elements(seite,".kGfYTK")[c(rep(FALSE, 3), TRUE)][seq(1:number)]%>% html_text() %>% as.data.frame()
  datum<-gsub("https://charts.spotify.com/charts/view/regional-de-weekly/","",page) %>% as.Date("%Y-%m-%d") %>% as.data.frame() # Erzeuge ein Datum aus der page-URL und speichere es als Datenspalte
  
  chart<-cbind(ids, position, titles, artists, streams, datum) # binde die Datenspalten zusammen
  names(chart)<-c("track.id", "track.position", "track.name", "track.artist", "track.streams", "chart.date") # gib den Datenspalten Namen
  return(chart)
}

########## MAIN PROGRAM

rD<-rsDriver(browser="chrome",port=4234L,chromever="106.0.5249.61") # Öffnet Selenium Browser
remDr<-rD[["client"]] # provides access to URL in Browser
remDr$open()
remDr$navigate("https://charts.spotify.com/charts/view/regional-de-weekly/latest")

########### Einloggen händisch nötig

startdate<-as.Date("2017/01/05")
enddate<-as.Date("2022/10/06")

dates_weekly<-seq(startdate,enddate,by="week")
dates_monthly<-dates_weekly[seq(1, length(dates_weekly), 4)] # choose every fourth week for monthly charts
urls_weekly<-paste0("https://charts.spotify.com/charts/view/regional-de-weekly/",dates_weekly)
urls_monthly<-paste0("https://charts.spotify.com/charts/view/regional-de-weekly/",dates_monthly)

top200_weekly_charts<-map_df(urls_weekly,spotiscrape, 200) %>% mutate(obs=rownames(.),.before=1)
top200_weekly_charts$time<-as.integer(difftime(top200_weekly_charts$chart.date, startdate, units="weeks")+1)
top30_monthly_charts<-map_df(urls_monthly,spotiscrape, 30) %>% mutate(obs=rownames(.),.before=1)
top30_monthly_charts$time<-rep(1:(nrow(top30_monthly_charts)/30), each=30)

remDr$close()
rD[["Server"]]$stop()

saveRDS(top200_weekly_charts, file = "top200_weekly_charts_germany.rds")
saveRDS(top30_monthly_charts, file = "top30_monthly_charts_germany.rds")
