rm(list=ls()) # workspace

library(tidyverse)  # import basic functions
library(spotifyr) # import spotify wrapper
library(stringr)

connect_spotify<-function(pass)     #Visumbeantragung bei API
{              
  cat("Connecting with SPotify-API","\n")
  Sys.setenv(SPOTIFY_CLIENT_ID=pass[1])
  Sys.setenv(SPOTIFY_CLIENT_SECRET=pass[2])
  access_token<-get_spotify_access_token()
  return(access_token)
}


pull_track_infos<-function(ids, token)
{
  liste<-c()                                            # Liste als leeren Vektor initialisieren
  anfang<-1
  stepsize<-50
  repeat
  {
    ende<-min(c(anfang+stepsize-1,length(ids)))    # in 50er Schritten (API-Limit) bis Listenende
    cat("Getting track-infos for track",anfang,"-",ende,"\n")
    ergebnis<-get_tracks(ids[anfang:ende],authorization=token)  # Trackinfos für aktuellen 50er-Block abholen
    liste<-rbind(liste,ergebnis)                                  # an bisherige Liste die Ergebnisse unten ankleben
    anfang<-anfang+stepsize
    if (anfang>length(ids)) {break}              #aussteigen wenn Listenende erreicht
  }
  liste$artist.name<-sapply(liste$artists,'[[','name') %>% sapply('[[',1) #Neue Variable für Artists-names, vorerst nur 1. Artist (aus Liste auspacken)
  liste$artist.id<-sapply(liste$artists,'[[','id') %>% sapply('[[',1) #Neue Variable Artists-ids, vorerst nur 1. Artist (aus Liste auspacken)
  
  liste<-select(liste,track.artistlist=artists,artist.name,artist.id,
                track.explicit=explicit,
                track.popularity=popularity, 
                track.isrc=external_ids.isrc,
                track.number=track_number, 
                album.id,album.name) #auswählen und umbenennen zu behaltender Variablen aus Datensatz "Liste"
  return(liste)
}

pull_artist_infos<-function(ids, token)
{
  liste<-c()
  anfang<-1
  stepsize<-50
  repeat
  {
    ende<-min(c(anfang+stepsize-1,length(ids)))    # in 50er Schritten (API-Limit) bis Listenende
    cat("Getting artist-infos for track",anfang,"-",ende,"\n")
    ergebnis<-get_artists(ids[anfang:ende], authorization=token)  # Trackinfos für aktuellen 50er-Block abholen
    liste<-rbind(liste,ergebnis)                                  # an bisherige Liste die Ergebnisse unten ankleben
    anfang<-anfang+stepsize
    if (anfang>length(ids)) {break}              #aussteigen wenn Listenende erreicht
  }
  
  liste<-select(liste, 
                artist.genres=genres,
                artist.genre=genres,
                artist.popularity=popularity, 
                artist.followers=followers.total)

  liste$artist.genre<-lapply(liste$artist.genre, '[[', 1)

  return(liste)
}

pull_album_infos<-function(ids, token)
{
  liste<-c()                                            # Liste als leeren Vektor initialisieren
  anfang<-1
  stepsize<-20
  repeat
  {
    ende<-min(c(anfang+stepsize-1,length(ids)))    # in 20er Schritten (API-Limit) bis Listenende
    cat("Getting album-infos for track",anfang,"-",ende,"\n")
    ergebnis<-get_albums(ids[anfang:ende])
    liste<-rbind(liste, ergebnis)               # album-infos der aktuellen 20er Blöcke aus der Spotify API holen
    anfang<-anfang+stepsize                     
    if (anfang>length(ids)) {break}             # aussteigen wenn Listenende erreicht
  }
  liste<-select(liste, 
                album.label=label,              # Label-Spalte auswählen und in "album.label" umbennen
                album.upc = external_ids.upc,
                album.trackcount = total_tracks,
                album.releasetype = type,
                album.popularity=popularity,
                album.release_date = release_date
                )       
  return(liste)
}

pull_audio_features<-function(ids, token)
{
  liste<-c()                                            # Liste als leeren Vektor initialisieren
  anfang<-1
  stepsize<-20
  repeat
  {
    ende<-min(c(anfang+stepsize-1,length(ids)))    # in 20er Schritten (API-Limit) bis Listenende
    cat("Getting audio-features for track",anfang,"-",ende,"\n")
    ergebnis<-get_track_audio_features(ids[anfang:ende])
    liste<-rbind(liste, ergebnis)
    anfang<-anfang+stepsize
    if (anfang>length(ids)) {break}             # aussteigen wenn Listenende erreicht
  }
  liste<-select(liste, # alles außer die folgenden auswählen
                -analysis_url,
                -track_href,
                -uri,
                -id,
                -type,
                f.danceability = danceability,
                f.energy = energy,
                f.key = key,
                f.loudness = loudness,
                f.mode = mode,
                f.speechiness = speechiness,
                f.acousticness = acousticness,
                f.instrumentalness = instrumentalness,
                f.liveness = liveness,
                f.valence = valence,
                f.tempo = tempo,
                f.time_signature = time_signature,
                f.duration_ms = duration_ms
               )
  return(liste)
}

# Funktion zum Auspacken der Artistliste um somit artistbasierten statt trackbasierten Datensatz erzeugen
expand_artists<-function(trackframe){
  trackframe<-select(trackframe,-artist.name,-artist.id) # alte vorläufige Artistnamen und IDs löschen  
  trackframe<-unnest(trackframe,cols=track.artistlist) #auspacken der verschachtelten Listenelemente von artist.list 
  trackframe<-rename(trackframe,artist.name=name,artist.id=id,) # neue Artistnamen und IDs richtig benennen
  trackframe<-select(trackframe,-href,-type,-uri,-external_urls.spotify) # überflüssige Variablen löschen
  return(trackframe)
}


###############

# choose and load data set
#filename<-"top200_weekly_charts_germany.rds"
filename<-"top30_monthly_charts_germany.rds"
tracklist<-read_rds(paste0("data/", filename))
tracklist<-head(tracklist, 10)

# set spotify Client ID and Client Secret
passport<-c("6cba647b5e724401b9a518c6745df78e","9edd34ffbf884f33b4a5b47857c29898")
token<-connect_spotify(passport)                      # Visum bei der API beantragen

# pull infos
track_infos<-pull_track_infos(tracklist$track.id, token) # pull track infos with spotify track-ids from scraped list
artist_infos<-pull_artist_infos(track_infos$artist.id, token) # pull artist infos with spotify track-ids from scraped list
album_infos<-pull_album_infos(track_infos$album.id, passport) # pull album infos with spotify album-ids that we got from pulling the track infos
track_features<-pull_audio_features(tracklist$track.id, passport) # pull track features with spotify track-ids from scraped list

# create trackbased and artist based lists
ergebnisse_trackbasiert<-cbind(tracklist, track_infos, artist_infos, album_infos, track_features)
ergebnisse_artistbasiert<-expand_artists(ergebnisse_trackbasiert)

# get stats
artists.brutto<-nrow(ergebnisse_artistbasiert)  # get total number of artists (incl. doublets)
artists.netto<-length(unique(ergebnisse_artistbasiert$artist.name)) # get number of artists without doublets
tracks.brutto<-nrow(ergebnisse_trackbasiert)  # get total number of tracks (incl. doublets)
tracks.netto<-nrow(distinct(tracklist, track.name, track.artist, .keep_all = T)) # get number of tracks without dounlets
labels.netto<-length(unique(ergebnisse_trackbasiert$album.label)) # get number of labels without doublets

# write data sets
#write_rds(ergebnisse_trackbasiert,paste0("data/spotified_tracks_",filename))
#write_rds(ergebnisse_artistbasiert,paste0("data/spotified_artists_",filename))

cols<-colnames(ergebnisse_artistbasiert)
albums_spotify <- get_albums(ergebnisse_trackbasiert$album.id[1])

view(ergebnisse_artistbasiert)
