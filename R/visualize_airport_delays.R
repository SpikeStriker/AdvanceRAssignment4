#' @name visualize_airport_delays
#' @title A function to visualise mean flight delays
#' @description A function to visualise mean flight delays across different airports using nycflights13
#' @import dplyr
#' @import ggplot2
#' @import gridExtra
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom gridExtra grid.arrange
#' @import nycflights13
#' @import maps
#' @import mapproj
#' @export
#' 
#' @examples
#' visualize_airport_delays()
#' @seealso
#' \url{https://nycflights13.tidyverse.org/index.html}

visualize_airport_delays<-function(){
  dt1<-nycflights13::airports %>% 
    dplyr::distinct(faa,name, lat, lon, .keep_all = FALSE) %>% 
    dplyr::left_join(dplyr::select(nycflights13::flights, dep_delay, origin),
              by = c("faa" = "origin")) %>%
    dplyr::group_by(faa,name, lat, lon) %>%
    dplyr::summarize(MeanFlightDepDelay = mean(dep_delay, na.rm=TRUE),.groups = 'drop') %>%
    dplyr::left_join(dplyr::select(nycflights13::flights, arr_delay, dest),
              by = c("faa" = "dest")) %>%
    dplyr::group_by(name, lat, lon, MeanFlightDepDelay) %>%
    dplyr::summarize(MeanFlightArrDelay = mean(arr_delay, na.rm=TRUE),.groups = 'drop') %>%
    dplyr::filter((!is.na(MeanFlightDepDelay))|(!is.na(MeanFlightArrDelay))) %>%
    dplyr::arrange(desc(MeanFlightDepDelay),desc(MeanFlightArrDelay))
  
  p1<-ggplot(dt1 %>% dplyr::filter(!is.na(MeanFlightArrDelay)),aes(x=lon,y=lat,label=name)) +
    geom_polygon(data=map_data("state"),aes(x=long,y=lat,group=group), 
                 inherit.aes = FALSE) +
    geom_point()+
    geom_text(check_overlap=TRUE,size=2,fontface="bold")+
    coord_map(xlim=c(-125,-70),ylim=c(25,50))+
    theme_classic() + 
    scale_color_distiller(palette="Spectral") + 
    aes(color = MeanFlightArrDelay)+
    labs(title="Mean Filght Arival Delay to NYC based Airport of Origin",
         x="Longitude",
         y="Latitude",
         colour="Mean Arrival Delay (min)")+
    theme(text = element_text(size=8))
  
  p2<-ggplot(dt1 %>% dplyr::filter(!is.na(MeanFlightDepDelay)),aes(x=lon,y=lat,label=name)) +
    geom_polygon(data=map_data("state"),aes(x=long,y=lat,group=group), 
                 inherit.aes = FALSE) +
    geom_point()+
    geom_text(check_overlap=TRUE,size=2,fontface="bold")+
    coord_map(xlim=c(-74.25,-73.75),ylim=c(40.6,40.85))+
    theme_classic() + 
    scale_color_distiller(palette="Spectral") + 
    aes(color = MeanFlightDepDelay)+
    labs(title="Mean Filght Departure Delay From NYC",
         caption="Data based on nycflights13",
         x="Longitude",
         y="Latitude",
         colour="Mean Departure Delay (min)")+
    theme(text = element_text(size=8))
  grid.arrange(p1, p2, ncol=1)
  }