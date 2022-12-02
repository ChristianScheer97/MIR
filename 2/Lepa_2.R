rm(list=ls()) # workspace

# Example Script for Scraping the global charts from Spotify

library(tidyverse)  # import basic functions
library(rvest) # import scraping functions
library(RSelenium) # import scraping for dynamic websites


spotiscrape<-function(page){
  remDr$navigate(page)   # gehe auf die Webseite "page"
  cat("Extracting data from page",page,"\n")
  Sys.sleep(2)  # warte 2 Sekunden
  seite<-read_html(remDr$getPageSource()[[1]]) # besorge Dir dynamische generierte Seite aus Browser
  links<-html_elements(seite,"a") %>% html_attr("href")  # ziehe dir alle href-Elemente aus den a-Elementen der Seite
  spotify_id<-links[grep('track',links)][c(T,F)]         # nimm nur die Links welche "track" enthalten und nur jeden zweiten
  ids<-gsub("https://open.spotify.com/track/","",spotify_id) %>% as.data.frame()  # entferne den URL-Teil und speichere es als Datenspalte
  position<-html_elements(seite,".hgLxdb") %>% html_text() %>% as.integer() %>% as.data.frame() #nimm Dir Text aus allen hgLxdb-Elememten, wandle in Integer und speichere als Datenspalte
  datum<-gsub("https://charts.spotify.com/charts/view/regional-global-weekly/","",page) %>% as.Date("%Y-%m-%d") %>% as.data.frame() # Erzeuge ein Datum aus der page-URL und speichere es als Datenspalte
  chart<-cbind(position,ids,datum) # binde die drei Datenspalten zusammen
  names(chart)<-c("position","spotify_id","date") # gib den Datenspalten Namen
  return(chart)
}

########## MAIN PROGRAM



rD<-rsDriver(browser="chrome",port=4234L,chromever="106.0.5249.61") # Öffnet Selenium Browser
remDr<-rD[["client"]] # provides access to URL in Browser
remDr$open()
remDr$navigate("https://charts.spotify.com/charts/view/regional-global-weekly/latest")

########### Einloggen händisch nötig


startdate<-as.Date("2017/01/05")
enddate<-as.Date("2022/10/06")
dates_weekly<-seq(startdate,enddate,by="week")
urls<-paste0("https://charts.spotify.com/charts/view/regional-global-weekly/",dates_weekly)

weekly_charts<-map_df(urls,spotiscrape)





