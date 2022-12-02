rm(list=ls()) # workspace

# Example Script for enriching chart data with spotify features

library(tidyverse)  # import basic functions
library(spotifyr) # import spotify wrapper

connect_spotify<-function(pass){                   #Visumbeantragung bei API
  cat("Connecting with SPotify-API","\n")
  Sys.setenv(SPOTIFY_CLIENT_ID=pass[1])
  Sys.setenv(SPOTIFY_CLIENT_SECRET=pass[2])
  access_token<-get_spotify_access_token()
  return(access_token)
}


pull_track_infos<-function(tracklist)
{
  token<-connect_spotify(passport)                      # Visum bei der API beantragen
  liste<-c()                                            # Liste als leeren Vektor initialisieren
  anfang<-1
  stepsize<-50
  repeat
  {
    ende<-min(c(anfang+stepsize-1,length(tracklist$track.id)))    # in 50er Schritten (API-Limit) bis Listenende
    cat("Getting track-infos for track",anfang,"-",ende,"\n")
    ergebnis<-get_tracks(tracklist$track.id[anfang:ende],authorization=token)  # Trackinfos für aktuellen 50er-Block abholen
    liste<-rbind(liste,ergebnis)                                  # an bisherige Liste die Ergebnisse unten ankleben
    anfang<-anfang+stepsize
    if (anfang>length(tracklist$track.id)) {break}              #aussteigen wenn Listenende erreicht
  }
  liste$artist.name<-sapply(liste$artists,'[[','name') %>% sapply('[[',1) #Neue Variable für Artists-names, vorerst nur 1. Artist (aus Liste auspacken)
  liste$artist.id<-sapply(liste$artists,'[[','id') %>% sapply('[[',1) #Neue Variable Artists-ids, vorerst nur 1. Artist (aus Liste auspacken)
  
  liste<-select(liste,track.artistlist=artists,artist.name,artist.id,
                track.explicit=explicit,track.popularity=popularity, track.isrc=external_ids.isrc,
                track.number=track_number,track.duration_ms=duration_ms, 
                track.popularity=popularity,album.id,album.name) #auswählen und umbenennen zu behaltender Variablen aus Datensatz "Liste"
  return(liste)
}

pull_album_infos<-function(ids)
{
  token<-connect_spotify(passport)                      # Visum bei der API beantragen
  liste<-c()                                            # Liste als leeren Vektor initialisieren
  anfang<-1
  stepsize<-20
  repeat
  {
    ende<-min(c(anfang+stepsize-1,length(ids)))    # in 50er Schritten (API-Limit) bis Listenende
    cat("Getting album-infos for track",anfang,"-",ende,"\n")
    ergebnis<-get_albums(ids[anfang:ende])
    liste<-rbind(liste, ergebnis)
    anfang<-anfang+stepsize
    if (anfang>length(ids)) {break}              #aussteigen wenn Listenende erreicht
  }
  liste<-select(liste, album.label=label)
  return(liste)
}

# Funktion zum Auspacken der Artistliste um somit artistbasierten statt Trackbasierten Datensatz erzeugen
expand_artists<-function(trackframe){
  trackframe<-select(trackframe,-artist.name,-artist.id) # alte vorläufige Artistnamen und IDs löschen  
  trackframe<-unnest(trackframe,cols=track.artistlist) #auspacken der verschachtelten Listenelemente von artist.list 
  trackframe<-rename(trackframe,artist.name=name,artist.id=id,) # neue Artistnamen und IDs richtig benennen
  trackframe<-select(trackframe,-href,-type,-uri,-external_urls.spotify) # überflüssige Variablen löschen
  return(trackframe)
}




###############

filename<-"top30_monthly_charts_germany.rds"

#filename<-"top200_monthly_charts_germany.rds"

tracklist<-read_rds(filename)

print(tracklist)

passport<-c("881cc29fc0b54ed0a0cc0143639027dd","078376740a274417995f9eaef7984687")

track_infos<-pull_track_infos(tracklist)
album_infos<-pull_album_infos(track_infos$album.id)
ergebnisse_trackbasiert<-cbind(tracklist, track_infos, album_infos)
ergebnisse_artistbasiert<-expand_artists(ergebnisse_trackbasiert)

artists.brutto<-nrow(ergebnisse_artistbasiert)  # get total number of artists (incl. doublets)
artists.netto<-length(unique(ergebnisse_artistbasiert$artist.name)) # get number of artists without doublets

tracks.brutto<-nrow(ergebnisse_trackbasiert)  # get total number of tracks (incl. doublets)
tracks.netto<-nrow(distinct(tracklist, track.name, track.artist, .keep_all = T)) # get number of tracks without doublets

labels.brutto<-length(ergebnisse_trackbasiert$album.label)
print(ergebnisse_trackbasiert$album.label)
labels.netto<-length(unique(ergebnisse_trackbasiert$album.label)) # get number of labels without doublets

write_rds(ergebnisse_trackbasiert,paste0("spotified_tracks_",filename))
write_rds(ergebnisse_artistbasiert,paste0("spotified_artists_",filename)
          

#######################################################################################
#plotting 30 Monthly

#Duration

top30_spotified<-read_rds("spotified_tracks_top30_monthly_charts_germany.rds")

duration_s<-round(top30_spotified$track.duration_ms/1000,digits=2) #Convert to Seconds

mean_duration_s<-round(mean(duration_s),digits=2)
sd_duration_s<-round(sd(duration_s),digits=2)

hist(duration_s,
     main="Track Duration \n Top 30 monthly Germany",
     xlab="Track Duration in s",
     ylab="Quantity",
     col="darkmagenta",
     breaks=20
)

abline(v=mean_duration_s,     # Add line for mean
       col="red",
       lwd=3)

text(x=300,y=240,paste("Mean = ", mean_duration_s, " s"),col="red",pos=4)
text(x=300,y=240*0.9,paste("SD = ",sd_duration_s," s"),pos=4)

#Streams

streams<-top30_spotified$track.streams
streams<-round(as.numeric(gsub(",", "", streams))/1000000,digits=2)

mean_streams<-round(mean(streams),digits=2)
sd_streams<-round(sd(streams),digits=2)

hist(streams,
     main="Streams \n Top 30 monthly Germany",
     xlab="Million Streams",
     ylab="Quantity",
     xlim=c(0,7),
     col="darkmagenta",
     breaks=50
)

abline(v=mean_streams, # Add line for mean
       col="red",
       lwd=3)


text(x=4.5,y=350,paste("Mean = ",mean_streams," Mio. Streams"),col="red",pos=4)
text(x=4.5,y=350*0.9,paste("SD = ",sd_streams," Mio. Streams"),pos=4)

#Popularity

popularity<-top30_spotified$track.popularity

mean_popularity<-round(mean(popularity),digits=2)
sd_popularity<-round(sd(popularity),digits=2)

hist(popularity,
     main="Popularity \n Top 30 monthly Germany",
     xlab="Popularity Index",
     ylab="Quantity",
     col="darkmagenta",
     breaks=50
)

abline(v=mean_popularity, # Add line for mean
       col="red",
       lwd=3)

text(x=80,y=300,paste("Mean = ",mean_popularity),col="red",pos=4)
text(x=80,y=300*0.9,paste("SD = ",sd_popularity),pos=4)


#######################################################################################
#plotting 200 Weekly


#Duration

top200_spotified<-read_rds("spotified_tracks_top200_weekly_charts_germany.rds")
 
duration_s<-round(top200_spotified$track.duration_ms/1000,digits=2) #Convert to seconds

mean_duration_s<-round(mean(duration_s),digits=2)
sd_duration_s<-round(sd(duration_s),digits=2)

hist(duration_s,
     main="Track Duration \n Top 200 weekly Germany",
     xlab="Track Duration in s",
     ylab="Quantity",
     col="darkmagenta",
     xlim=c(0,400),
     breaks=100
)

abline(v=mean_duration_s,     # Add line for mean
       col="red",
       lwd=3)

text(x=300,y=7000,paste("Mean = ", mean_duration_s, " s"),col="red",pos=4)
text(x=300,y=7000*0.9,paste("SD = ",sd_duration_s," s"),pos=4)

#Streams

streams<-top200_spotified$track.streams
streams<-round(as.numeric(gsub(",","",streams))/1000000,digits=2) #Convert to Seconds

mean_streams<-round(mean(streams),digits=2)
sd_streams<-round(sd(streams),digits=2)

hist(streams,
     main="Streams \n Top 200 weekly Germany",
     xlab="Million Streams",
     ylab="Quantity",
     xlim=c(0,5),
     col="darkmagenta",
     breaks=100
)

abline(v = mean_streams, # Add line for mean
       col = "red",
       lwd = 3)


text(x=3.5,y=10000,paste("Mean = ", mean_streams," Mio. Streams"),col="red",pos=4)
text(x=3.5,y=10000*0.9,paste("SD = ", sd_streams," Mio. Streams"),pos=4)

#Popularity

popularity<-top200_spotified$track.popularity

mean_popularity<-round(mean(popularity),digits=2)
sd_popularity<-round(sd(popularity),digits=2)

hist(popularity,
     main="Popularity \n Top 200 weekly Germany",
     xlab="Popularity Index",
     ylab="Quantity",
     col="darkmagenta",
     breaks=50
)

abline(v=mean_popularity, # Add line for mean
       col="red",
       lwd=3)

text(x=80,y=8000,paste("Mean = ",mean_popularity),col="red",pos=4)
text(x=80,y=8000*0.9,paste("SD = ",sd_popularity),pos=4)










