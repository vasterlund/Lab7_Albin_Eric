#'@title Do you want to plot the mean delay time?
#'@description Plats an interactive plot that displays the mean delay time for every airport
#'
#'
#'
#' @field no argument
#' 
#'@examples
#' visualize_airport_delays()
#' @export 
#' 
#' 

visualize_airport_delays <- function(){
  
  
  requireNamespace("nycflights13")
  requireNamespace("dplyr")
  requireNamespace("magrittr")
  
  #requireNamespace("plotly")
  
  airports<-nycflights13::airports
  flights<-nycflights13::flights
  
  data <- dplyr::inner_join(airports, flights, by = c("faa" = "dest")) 
  
  #doin some dplyr stuff
  
  vect <- data %>%
    group_by(faa) %>%
    summarize(Mean = mean(arr_delay,na.rm = TRUE))
  
  coordin <- data %>%
    group_by(faa) %>%
    summarize(Coordinates = paste0("lat = ", lat[1],", lon = ", lon[1], collapse = " "))
  
  #####
  lat <- data %>%
    group_by(faa) %>%
    summarize(lat = lat[1])
  lon <- data %>%
    group_by(faa) %>%
    summarize(lon = lon[1])
  
  
  
  #####
  
  graph_data <- data.frame(vect, coordin[,2],lat,lon)
  
  ###PLOTTA
  p<-ggplot2::ggplot(graph_data, aes(x=lon, y=lat, color=Mean)) + geom_point(size=3) + 
    scale_colour_gradient2(low="blue",high="red",mid="grey") +
    #scale_color_gradient(low="white", high=" black")+
    theme_bw() + theme(axis.title.y = element_text(angle = 0, hjust = 1))+ 
    ggtitle("Visualising the delay of the arrival")+ labs(x ="East (Longitude)" , y = "North (Latitude)")
  
  # p <- ggplot(graph_data, aes(x = faa, y = Mean, label = Coordinates)) + 
  #   geom_point() + labs(x = "Airports") + theme_bw() +
  #   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
  #suppressMessages(ggplotly(p))
  p
  
  
}
