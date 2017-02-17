#' @description Function to read FARS file.
#'
#' This function uses readr and dplyr packages.
#' It takes a CSV file as an input filename parameter.
#' If the file does not exist in the working directory, this function stops with an output message that the file does not exist.
#' If the file exists, this function reads it into dataframe object.
#' It suppresses messages and progress bar while reading the file.
#' 
#' @import(tidyr)
#' @import(dplyr)
#' @import(maps)
#' @import(mapdata)
#' 
#' Print "fars_read" 
#' @description Function to load a CSV dataset into a dataframe
#' @details Dataset must accomplish the FARS structure
#'
#' @param filename Path to file
#' 
#' @return The dataframe generated   
#'         Error if file doesn't exist
#'
#' @examples
#' fars_read('filename')
#' fars_read('dir1/dir2/filename')
#'
#'

library(tidyr)
library(dplyr)
library(lubridate)
library(readr)

geom_load_file <- function(filename="ebtrk_atlc_1988_2015.txt") {
   ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3, 4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
   ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                            "hour", "year", "latitude", "longitude",
                            "max_wind", "min_pressure", "rad_max_wind",
                            "eye_diameter", "pressure_1", "pressure_2",
                            paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                            paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                            paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                            "storm_type", "distance_to_land", "final")

   ext_tracks <- read_fwf(filename, fwf_widths(ext_tracks_widths, ext_tracks_colnames), na = "-99")
   
   res <- ext_tracks %>% 
          unite(datetime, year, month, day, hour) %>%
          mutate(date = ymd_h(datetime)) %>%
          select(storm_id, storm_name, date, latitude, longitude, starts_with("radius")) %>%
          gather(TXTRadius, Distance, radius_34_ne:radius_64_nw, na.rm = TRUE) %>%
          separate(TXTRadius, c("TXT1","wind_speed", "Orden"), sep="_") %>%
          unite(TXTRadius, TXT1, wind_speed, remove=FALSE) %>%
          spread(Orden, Distance) %>%
          mutate(wind_speed = as.factor(wind_speed)) %>%
          select(-starts_with("TXT")) %>%
          mutate(longitude=longitude * -1.0)
   
}

geom_get_observation <- function(data, name="IKE", fecha="2008-09-12", hora="18:00:00") {
  dt <- ymd_hms(paste(fecha, hora))
  data %>% filter(storm_name == name & date == dt)
}
