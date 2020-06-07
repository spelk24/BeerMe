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
      column(4,
        selectInput(ns("BeerSelection"),"Beers", choices = unique(beer_full_df$Brand))
        ),
    ),
    fluidRow(
      column(6,plotOutput(ns("Neighbor_Zoom"), width = "100%", height = "500px")),
      column(6,plotlyOutput(ns("Scatter"), width = "90%", height = "500px")),
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
      labs(title = "UMAP Respresentation of Beer Data",
           x = "UMAP 1",
           y = "UMAP 2") +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      scale_size_manual(values = c(1.75,2.75)) +
      theme(plot.title = element_markdown(size = 14),
            plot.subtitle = element_markdown(size = 10),
            plot.caption = element_markdown(),
            legend.position = "none",
            axis.title = element_markdown(size = 10),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.major = element_blank(),
            axis.line = element_line(color = "#d9d9d9",size = 1.5))
    
    ggplotly(gg_umap, tooltip = "text") %>%
      config(displayModeBar = F) %>%
      layout(title = list(text = paste0("Low Dimensional Respresentation of Beer Data",
                                        "<br>",
                                        "<sup>",
                                        "UMAP Algoritm: See Methodology tab for tutorial",
                                        "</sup>")))
  })
    
  output$Neighbor_Zoom <- renderPlot({
      
      colors <- c(
        "#f6c101",
        "#c96e12"
      )
      selection <- c(input$BeerSelection)
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
        x = c(0,.72,.65,-.65,-.72,0),
        y = c(0,.72,-.65,-.65,.72,1.2),
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
        xlim(-1.25,1.25) +
        ylim(-1.25,1.25) +
        labs(title = glue("If you like ","<b style='color:#c96e12'>",selection,"</b> you might also enjoy these ","<b style='color:#c96e12'>similar beers</b>")
             ) +
        theme(plot.title = element_markdown(size = 14, hjust = .1, vjust = .1),
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
# mod_UMAP_Scatter_ui("UMAP_Scatter_ui_1")
    
## To be copied in the server
# callModule(mod_UMAP_Scatter_server, "UMAP_Scatter_ui_1")
 
