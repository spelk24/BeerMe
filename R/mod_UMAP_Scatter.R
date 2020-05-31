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
    col_3(
      selectInput(ns("BeerSelection"),"Beers", choices = unique(beer_full_df$Brand))
      ),
    
    col_12(plotlyOutput(ns("Scatter"), width = "90%", height = "480px")
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
#' @import glue
#' @import stringr
mod_UMAP_Scatter_server <- function(input, output, session){
  ns <- session$ns
  output$Scatter <- renderPlotly({
    
    ### --- Logic that can be moved to helpers.R --- ###
    colors <- c(
      "#f6c101",
      "#c96e12"
    )
    selection <- input$BeerSelection
    
    idxs <- beer_nn %>% filter(Brand %in% selection, Neighbor_Rk <= 5) %>% pull(Neighbor_Idx)
    neighbor_points <- beer_full_df %>%
      filter(Brand != selection) %>%
      slice(idxs) %>%
      pull(Brand)
    
    neighbor_points_abv <- beer_full_df %>%
      filter(Brand != selection) %>%
      slice(idxs) %>%
      pull(ABV)
    
    beer_full_df$Selected <- if_else(beer_full_df$Brand %in% neighbor_points,"Y","N")
    
    ### --- UMAP Plot--- ###
    
    gg_umap <- ggplot(data = beer_full_df,aes(x = UMAP_X,
                                              y = UMAP_Y,
                                              fill = Selected,
                                              color = Selected,
                                              size = Selected,
                                              text = paste0(Brand,"<br>",Brand_Style,"<br>","ABV: ",ABV))) +
      geom_jitter(width = 1,
                  height = 1,
                  alpha = .7,
                  show.legend = FALSE) +
      labs(title = "UMAP Respresentation of Beer Data") +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      scale_size_manual(values = c(1.75,2.75)) +
      theme(plot.title = element_markdown(size = 14),
            plot.subtitle = element_markdown(size = 10),
            plot.caption = element_markdown(),
            legend.position = "none",
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.major = element_line(colour = "#d9d9d9",size = .5))
    
    ggplotly(gg_umap, tooltip = "text") %>%
      config(displayModeBar = F) %>%
      layout(title = list(text = paste0("BEER ME Nearest Neighbors",
                                        "<br>",
                                        "<sup>",
                                        "UMAP 2-D representation of beer ingreidents",
                                        "</sup>")))
  })
 
}
    
## To be copied in the UI
# mod_UMAP_Scatter_ui("UMAP_Scatter_ui_1")
    
## To be copied in the server
# callModule(mod_UMAP_Scatter_server, "UMAP_Scatter_ui_1")
 
