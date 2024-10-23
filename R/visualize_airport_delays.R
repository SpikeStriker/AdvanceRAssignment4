#' @name visualize_airport_delays
#' @title A function to visualise mean flight delays
#' @description A function to visualise mean flight delays across different airports using nycflights13
#' @import nycflights13
#' @import dplyr
#' @import tidylog
#' @export
#' 
#' @examples
#' data("nycflights13")
#' 
#' visualize_airport_delays()
#' @seealso
#' \url{https://nycflights13.tidyverse.org/index.html}

visualize_airport_delays<-function(){
  data(nycflights13)
  x<-airports %>% 
    distinct(faa,name, lat, lon, .keep_all = FALSE) %>% 
    inner_join(select(flights, dep_delay, origin),
               by = c("faa" = "origin")) %>%
    group_by(name, lat, lon) %>%
    dplyr::summarize(Mean = mean(dep_delay, na.rm=TRUE))
  }

# library("nycflights13")
# library("dplyr")
# library("tidylog")

# head(airlines)
# head(airports)
# head(flights)


