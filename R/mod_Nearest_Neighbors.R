#' Nearest_Neighbors UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Nearest_Neighbors_ui <- function(id){
  ns <- NS(id)
  tagList(
    col_12(plotOutput(ns("Neighbor_Zoom"), width = "75%", height = "400px"))
  )
}
    
#' Nearest_Neighbors Server Function
#'
#' @noRd 
#' @import ggplot2
#' @import dplyr
#' @import plotly
#' @import ggtext
#' @import glue
#' @import stringr
mod_Nearest_Neighbors_server <- function(input, output, session){
  ns <- session$ns

  output$Neighbor_Zoom <- renderPlot({
    
    colors <- c(
      "#f6c101",
      "#c96e12"
    )
    selection <- c("Miller Lite")
    selection_abv <- beer_full_df %>%
      filter(Brand == selection) %>%
      pull(ABV)
    
    idxs <- beer_nn %>% filter(Brand %in% selection, Neighbor_Rk <= 5) %>% pull(Neighbor_Idx)
    neighbor_points <- beer_full_df %>%
      filter(Brand != selection) %>%
      slice(idxs) %>%
      pull(Brand)
    
    neighbor_points_abv <- beer_full_df %>%
      filter(Brand != selection) %>%
      slice(idxs) %>%
      pull(ABV)
    
    zoom_neighbor_data <- tibble(
      x = c(0,.72,.72,-.72,-.72,0),
      y = c(0,.72,-.72,-.72,.72,1.2),
      center_x = c(0,0,0,0,0,0),
      center_y = c(0,0,0,0,0,0),
      beer = c(selection,neighbor_points),
      abv = c(selection_abv,neighbor_points_abv)
    )
    # format label
    zoom_neighbor_data$abv = str_c(zoom_neighbor_data$abv,rep(c("%"),6),sep = "")
    zoom_neighbor_data$label = str_c(zoom_neighbor_data$beer,zoom_neighbor_data$abv,sep = "\nABV: ")
    
    neighbor_zoom <- ggplot(data = zoom_neighbor_data) +
      geom_segment(aes(x = x, y = y, xend = center_x, yend = center_y),
                   colour = colors[2]) +
      geom_point(aes(x = x, y = y),
                 size = 5,
                 colour = colors[2]) +
      geom_label(aes(x = x, y = y,label = label),
                 fill = colors[2],
                 color = "white",
                 label.padding = unit(0.45, "lines"),
                 size = 3) +
      xlim(-1.5,1.5) +
      ylim(-1.5,1.5) +
      labs(title = glue("If you like ","<b style='color:#c96e12'>",selection,"</b> you might<br>also enjoy these ","<b style='color:#c96e12'>similar beers</b>"),
           caption = "For detailed ingredients on each beer, go to the Beer Data tab") +
      theme(plot.title = element_markdown(size = 14, hjust = .5, vjust = .1),
            plot.subtitle = element_markdown(size = 10),
            plot.caption = element_markdown(hjust = .5),
            legend.position = "none",
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid = element_blank())
    neighbor_zoom
    
  })
}
    
## To be copied in the UI
# mod_Nearest_Neighbors_ui("Nearest_Neighbors_ui_1")
    
## To be copied in the server
# callModule(mod_Nearest_Neighbors_server, "Nearest_Neighbors_ui_1")
 
