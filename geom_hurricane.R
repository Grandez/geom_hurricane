#' @print "geom_hurricane"
#'
#'
#' Geom to show the wind radii of one observation of one Hurricane
#' 
#' @seealso  \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}
#'
#' @author Javier Gonzalez <javier.gonzalez.grandez@gmail.com> 
#' 
#' @import(ggplot2)
#' @import(geosphere)

library(ggplot2)
library(grid)
library(geosphere)

# Magic number. Number of meters by nautic mille
METTERS_BY_MILLE <- 1852

#' Private function to calculate the radio along X-axis
#' 
#' @param origin   A point in format Latitude, Longitude
#' @param distance Distance in milles nautiques
#' @param delta    Number of xunits by grade
#' @param scale    When applied extent to scale percentage of real radii
#'                 
#' @return the appropiate units of radii along X-axis
getLonRadius <- function(origin, distance, delta, scale=1) {
  p <- destPoint(origin, 0, METTERS_BY_MILLE * distance)
  diff <- p[,2] - origin[2] 
  res <- diff * delta * scale 
  return (res)
}

#' Private function to calculate the radio along Y-axis
#' 
#' @param origin   A point in format Latitude, Longitude
#' @param distance Distance in milles nautiques
#' @param delta    Number of yunits by grade
#' @param scale    When applied extent to scale percentage of real radii
#'                 
#' @return the appropiate units of radii along Y-axis
getLatRadius <- function(origin, distance, delta, scale) {
  p <- destPoint(origin, 90, METTERS_BY_MILLE * distance)
  diff <- p[,1] - origin[1] 
  res <- diff * delta * scale 
  return (res)  
}

#' Private function to draw the geom
#' 
#' @details 
#'    1.- On map calculate the postion of observation (Origin)
#'    2.- Create 4 viewports using this point
#'        +--------+--------+
#'        |        |        |
#'        |   v1   |   v3   |
#'        |        |        | 
#'        +--------o--------+
#'        |        |        |
#'        |   V2   |   v4   |
#'        |        |        |
#'        +--------+--------+
#'    3.- Draw on each view port a circle with origin = o and radius the appropiate radius
#'        depending of zoom of base map
#'        
#' @param name     The name of geom
#' @param geom     Geom parent
#' @param required_aes    the required aesthetics: x, y, r_ne, r_se, r_nw, r_sw
#' @param default_aes     default values for optional aesthetics: alpha, scale_radii, fill, colour
#' @param draw_key        function to draw legend
#' @param draw_panel      function to draw the geom
#'                 
#' @return grob
geomHurricane <- ggproto("geomHurricane", GeomPolygon,
                          required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),
                          default_aes = aes(shape=19, 
                                            alpha=0.5, 
                                            scale_radii=1.0,
                                            fill=c("red", "orange", "yellow"), 
                                            color=c("red", "orange", "yellow")),
                          draw_key = draw_key_polygon,
                          draw_panel = function(data, panel_scales, coord) {
                                
                                coords <- coord$transform(data, panel_scales)
                                scale <- panel_scales
                                origin <- c(data["x"][1,], data["y"][1,])
                                cc <- coords$colour

                                vx <- coords$x[1]
                                vy <- coords$y[1]
                                
                                deltaX <- abs(1 / diff(scale$x.range))
                                deltaY <- abs(1 / diff(scale$y.range))
                                
                                v1<- viewport(x=vx,y=vy,width=1,height=1,just=c("left","bottom"), clip="on")
                                v2<- viewport(x=vx,y=vy,width=-1,height=1,just=c("left","bottom"), clip="on")
                                v3<- viewport(x=vx,y=vy,width=-1,height=-0.75,just=c("left","bottom"), clip="on")
                                v4<- viewport(x=vx,y=vy,width=1,height=-0.75, just=c("left", "bottom"), clip="on")

                                c1 <- circleGrob(
                                  x = 0,
                                  y = 0,
                                  vp=v1,
                                  r = getLonRadius(origin, coords$r_ne, deltaX, coords$scale_radii),
                                  gp = gpar(col = cc, fill= cc, lty = "solid", lwd = 1, fontsize = 16, alpha = coords$alpha)
                                )
                                c2 <- circleGrob(
                                  x = 0,
                                  y = 0,
                                  vp=v2,
                                  r = getLonRadius(origin, coords$r_nw, deltaX, coords$scale_radii),
                                  
                                  gp = gpar(col = cc, fill= cc, lty = "solid", lwd = 1, fontsize = 16, alpha = coords$alpha)
                                )
                                c3 <- circleGrob(
                                    x = 0,
                                    y = 0,
                                    r = getLonRadius(origin, coords$r_sw, deltaX, coords$scale_radii),
                                    vp = v3,
                                    gp = gpar(col = cc, fill= cc, lty = "solid", lwd = 1, fontsize = 16, alpha = coords$alpha)
                                )
                                c4 <- circleGrob(
                                  x = 0,
                                  y = 0,
                                  r = getLonRadius(origin, coords$r_se, deltaX, coords$scale_radii),
                                  vp = v4,
                                  gp = gpar(col = cc, fill= cc, lty = "solid", lwd = 1, fontsize = 16, alpha = coords$alpha)
                                )
                                
                                grobTree(c1, c2, c3, c4)
                         }
                  )


#' Function to create the geom
#' 
#' @details 
#'    1.- On map calculate the postion of observation (Origin)
#'    2.- Create 4 viewports using this point
#'        +--------+--------+
#'        |        |        |
#'        |   v1   |   v3   |
#'        |        |        | 
#'        +--------o--------+
#'        |        |        |
#'        |   V2   |   v4   |
#'        |        |        |
#'        +--------+--------+
#'    3.- Draw on each view port a circle with origin = o and radius the appropiate radius
#'        depending of zoom of base map
#'        
#' @param data     The data frame with the observation to plot
#' @param aes      The aesthetics
#'                 Required
#'                    x = Longitude
#'                    y = Latitude
#'                    r_se = the maximum radial extent of winds of SE direction
#'                    r_ne = the maximum radial extent of winds of NE direction
#'                    r_sw = the maximum radial extent of winds of SW direction
#'                    r_nw = the maximum radial extent of winds of NW direction
#'                 Optional
#'                    fill = colors to fill
#'                    color = colors for lines
#'                    alpha = transparency
#'                    scale_radii = Percentaje of maximum radial extent
#'                    

geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  

  layer(
    geom = geomHurricane, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

