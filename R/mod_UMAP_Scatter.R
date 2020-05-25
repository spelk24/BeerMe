#' UMAP_Scatter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_UMAP_Scatter_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      selectInput(ns("BeerSelection"),"Beers", choices = unique(beer_full_df$Brand)),
      plotlyOutput(ns("Scatter"), width = "90%", height = "480px")
    )
  )
}
    
#' UMAP_Scatter Server Function
#'
#' @noRd 
#' @import ggplot2
#' @import dplyr
#' @import plotly
#' @import ggtext
mod_UMAP_Scatter_server <- function(input, output, session){
  ns <- session$ns
  output$Scatter <- renderPlotly({
    UMAPScatter <- ggplot(data = beer_full_df,aes(x = UMAP_X, y = UMAP_Y)) +
      geom_point(alpha = .7,
                 color = "blue",
                 show.legend = TRUE) +
      labs(title = "BEER ME Scatter",
           subtitle = "2-Dimensional Space from UMAP algorithm",
           x = "Dimension 1",
           y = "Dimension 2",
           caption = "See Methodology tab for details") +
      theme(plot.title = element_text(size = 14),
            legend.title = element_blank())
    ggplotly(UMAPScatter) %>%
      config(displayModeBar = F)
  })
 
}
    
## To be copied in the UI
# mod_UMAP_Scatter_ui("UMAP_Scatter_ui_1")
    
## To be copied in the server
# callModule(mod_UMAP_Scatter_server, "UMAP_Scatter_ui_1")
 
