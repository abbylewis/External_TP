#' map_val
#'
#' @param df input data frame
#' @param name name of the variable to be mapped
#'
#' @return ggplot object
#' @export
#'
#' @examples
map_val <- function(df, name) {
  for_states <- df%>%
    left_join(lat_long)
  states <- ne_states(returnclass = "sf",country = "United States of America")
  
  us_map_flux = ggplot(data = states) +
    geom_sf(fill = "white") +
    coord_sf(expand = FALSE, ylim = c(42, 47.5), xlim = c(-93,-86.5))+
    geom_point(data = for_states, 
               aes(Longitude_DD, Latitude_DD, fill = mean),
               shape = 21, color = "grey50", size = 2, stroke = .4)+
    theme_bw()+
    scale_fill_viridis(name = name, n.breaks = 4, trans = "log", labels = function(x) round(x, 2))+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "grey80"),
          legend.position = "bottom")
}


#' map_trend
#'
#' @param df input data frame
#' @param name name of the variable to be mapped
#'
#' @return ggplot object
#' @export
#'
#' @examples
map_trend <- function(df, name) {
  df <- df %>%
    left_join(lat_long) %>%
    filter(Latitude_DD > 42, 
           Latitude_DD < 47.5,
           Longitude_DD > -93,
           Longitude_DD < -86.5)
  min = min(df$trend)*10
  max = max(df$trend)*10
  abs_max = max(abs(c(min,max)))
  
  for_states <- df
  states <- ne_states(returnclass = "sf",country = "United States of America")
  
  us_map_flux = ggplot(data = states) +
    geom_sf(fill = "white") +
    coord_sf(expand = FALSE, ylim = c(42, 47.5), xlim = c(-93,-86.5))+
    geom_point(data = for_states, 
               aes(Longitude_DD, Latitude_DD, fill = trend*10),
               shape = 21, color = "grey50", size = 2, alpha  =.5, stroke = .4)+
    theme_bw()+
    #scale_fill_viridis(name = "Air temperature trend\n(ºC/decade)",
    #                   limits = c(min,max), option = "H")+
    scale_fill_gradientn(name = name,
                         colours = c("blue","white","red"),
                         limits = c(-abs_max, abs_max),
                         n.breaks = 4)+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          panel.background = element_rect(fill = "grey80"),
          legend.position = "bottom")
}