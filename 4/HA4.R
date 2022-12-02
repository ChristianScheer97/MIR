rm(list=ls()) # workspace

# Example Script for enriching spotify data with musicbrainz features

library(tidyverse)  # import basic functions
library(musicbrainz) # import musicbrainz functions
library(stringdist)  # import string comparison functions
library(rvest)


####################################################
# pull ids

pull_musicbrainz_artist_ids<-function(artistvector)
{
  ergebnis_liste<-c()
  cat("Suche Artists bei Musicbrainz...\n")
  for (i in 1:length(artistvector))
  {
    ergebnis<-search_artists(artistvector[i])
    cat("Gesucht:",artistvector[i],"  ")
    cat("Gefunden:",ergebnis$name[1],"\n")
    fund<-ergebnis[1,]
    fund$gesucht<-artistvector[i]
    fund$quality<-stringsim(artistvector[i],ergebnis$name[1],"jw")
    fund<-select(fund,gesucht,name,quality,everything())
    ergebnis_liste<-rbind(ergebnis_liste,fund)
  }
  ergebnis_liste$life_span_begin<-substr(ergebnis_liste$life_span_begin,1,4) %>% as.integer
  ergebnis_liste$life_span_end<-substr(ergebnis_liste$life_span_end,1,4) %>% as.integer
  ergebnis_liste<-select(ergebnis_liste,mb.gesucht=gesucht,mb.artist.name=name,mb.artist.quality=quality,
                         mb.artist.id=mbid,mb.artist.type=type,mb.artist.gender=gender,mb.artist.country=country,mb.artist.city=area_name,
                         mb.artist.birthyear=life_span_begin,mb.artist.deathyear=life_span_end,mb.artist.dead=life_span_ended)
  return(ergebnis_liste)
}

pull_musicbrainz_track_ids<-function(trackvector)
{
  ergebnis_liste<-c()
  cat("Suche Tracks bei Musicbrainz...\n")
  for (i in 1:nrow(trackvector))
  {
    title<-trackvector$track.name[i]
    isrc<-trackvector$track.isrc[i]
    ergebnis<-search_recordings(paste(title, " AND isrc:\"", isrc, "\"", sep = ""))
    ergebnis<-ergebnis[1,]
    if (is.na(ergebnis))
    {
      cat("keine isrc gefunden")
      ergebnis<-search_recordings(title)
      quality<-stringsim(toupper(trackvector$track.name[i]),toupper(ergebnis$title[1]),"jw")[1] #quality measurement with string-distance by Jero Winkler. Both Strings were converted to uppercase letters to avoid bigger string distance by upper and lowercase writing
    }
    else #Sets quality to 1, if track was found via ISRC
    {
      quality<-1.0
      fund<-ergebnis[1,]
    }
    cat("Gesucht: ", title,"  ")
    cat("Gefunden: ",ergebnis$title[1],"\n")
    fund$gesucht<-title
    fund$quality<-quality
    fund<-select(fund,gesucht,title,quality,everything())
    ergebnis_liste<-rbind(ergebnis_liste,fund)
  }
  ergebnis_liste<-select(ergebnis_liste, mb.track.id=mbid, mb.track.quality=quality)
  return(ergebnis_liste)
}

pull_musicbrainz_album_ids<-function(trackvector)
{
  ergebnis_liste<-c()
  cat("Suche Alben bei Musicbrainz...\n")
  for (i in 1:nrow(trackvector))
  {
    album.upc<-trackvector¢album.upc[i]
    album.title<-trackvector$album.name[i]
    album.label<-trackvector$album.label[i]
    track.artist<-trackvector$track.artist[i]
    ergebnis<-search_releases(paste(album.title, " AND label:\"", album.label, "\"", " AND artist:\"", track.artist, "\"", sep = ""))
    if (is.na(ergebnis))
    {
      ergebnis<-search_releases(paste(album.title, " AND artist:\"", track.artist, "\"", sep = ""))
      if (is.na(ergebnis))
      {
        ergebnis<-search_releases(paste(album.title))
      }
    }
    ergebnis<-ergebnis[1,]
    cat("Gesucht:", album.title, "  ")
    cat("Gefunden: ", ergebnis$title[1], "\n")
    fund<-ergebnis[1,]
    fund$gesucht<-album.title
    fund$quality<-stringsim(trackvector$album.name[i], ergebnis$title[1], "jw")[1]
    fund<-select(fund, gesucht,album.title=title,quality)
    ergebnis_liste<-rbind(ergebnis_liste,fund)
  }
  return(ergebnis_liste)
}

###################################################
# pull infos

pull_musicbrainz_artist_infos<-function(mbidvector)
{
  whitelist<-read_html('https://musicbrainz.org/genres')%>%html_elements('bdi')%>%html_text()%>%as.data.frame()
  whitelist<-rename(whitelist,genres=.)
  ergebnis_liste<-c()
  #whitelist<-read_csv("musicbrainz_genre_list.csv")
  cat("Hole Artists-Informationen bei Musicbrainz...\n")
  for (i in 1:length(mbidvector)){
    cat("Hole Infos für MBID",mbidvector[i],"\n")
    ergebnis<-lookup_artist_by_id(mbidvector[i],includes=c("tags"))
    fund<-ergebnis[1,"tags"][[1]][[1]]
    if ("tag_name"%in%colnames(fund)){
      fund<-filter(fund,tag_name%in%whitelist$genre)
      fund<-arrange(fund,-tag_count)[1,"tag_name"]
    }
    else
    {
      fund<-NA
    }
    ergebnis_liste<-rbind(ergebnis_liste,fund)
  }
  ergebnis_liste<-select(ergebnis_liste,
                         mb.artist.genre = tag_name)
  return(ergebnis_liste)
}

pull_musicbrainz_album_infos()<-function(mbidvector)
{
  cat("Hole Album-Informationen bei Musicbrainz...\n")
  ergebnis_liste<-c()
  for (i in 1:length(mbidvector))
  {
    ergebnis<-lookup_recording_by_id(mbidvector[i])
    fund<-ergebnis[1]
    ergebnis_liste<-rbind(ergebnis_liste,fund)
  }
  ergebnis_liste<-select(ergebnis_liste, 
                         album.mbid=mbid, 
                         album.date=date, 
                         album.country=country, 
                         album.barcode=barcode, 
                         album.track_count=track_count, 
                         album.type=release_group_primary_type)
  return(ergebnis_liste)
}

############# Main Program

#filename<-"spotified_tracks_top30_monthly_charts_germany.rds"
filename<-"spotified_artists_top30_monthly_charts_germany.rds"

tracklist<-read_rds(filename)

ergebnis_artist_ids<-pull_musicbrainz_artist_ids(tracklist$artist.name) # Get Musicbrainz Artist IDs
ergebnis_artist_infos<-pull_musicbrainz_artist_infos(ergebnis_liste$mb.artist.id) # Get Musicbrainz Artist Infos
ergebnis_track_ids<-pull_musicbrainz_track_ids(tracklist) # Get Musicbrainz Track IDs
ergebnis_album_ids_infos<-pull_musicbrainz_album_ids_and_infos(tracklist) # Get Musicbrainz Album IDs and Album Infos

ergebnis_liste<-cbind(tracklist, ergebnis_artist_ids, ergebnis_artist_infos, ergebnis_track_ids, ergebnis_album_ids_infos)

#ergebnis<-pull_musicbrainz_track_ids(tracklist)

#write_rds(ergebnis_liste,paste0("mb_tracks_",filename))
write_rds(ergebnis_liste,paste0("mb_artists_",filename))

summarize(ergebnis_liste,average_quality=mean(quality))

##### ausprobieren