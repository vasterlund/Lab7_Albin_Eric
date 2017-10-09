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
  
  requireNamespace("tidyverse")
  requireNamespace("nycflights13")
  requireNamespace("plotly")
  
  data(flights)
  data(airports)
  
  data <- inner_join(airports, flights, by = c("faa" = "dest")) 
  
  #doin some dplyr stuff
  
  vect <- data %>%
    group_by(faa) %>%
    summarize(Mean = mean(arr_delay,na.rm = TRUE))
  
  coordin <- data %>%
    group_by(faa) %>%
    summarize(Coordinates = paste0("lat = ", lat[1],", lon = ", lon[1], collapse = " "))
  
  
  
  graph_data <- data.frame(vect, coordin[,2])
  
  ###PLOTTA
  p <- ggplot(graph_data, aes(x = faa, y = Mean, label = Coordinates)) + 
    geom_point() + labs(x = "Airports") + theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
  
  suppressMessages(ggplotly(p))
  
  
}
