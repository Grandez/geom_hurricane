library(ggmap)

mapea <- function(observation=data1,zoom=6) {
  get_map("Louisiana", zoom = zoom) %>%
  ggmap(extent = "device") +
  
  geom_hurricane(data = observation,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed, scale_radii = 1)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow")) +
}
