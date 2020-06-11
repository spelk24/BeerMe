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
      column(6,tableOutput(ns("Neighbor_Table"))),
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
#' @import knitr
#' @import kableExtra
#' @import formattable
mod_UMAP_Scatter_server <- function(input, output, session){
  ns <- session$ns
  output$Scatter <- renderPlotly({
    
    ### --- Logic that can be moved to helpers.R --- ###
    colors <- c(
      "#fec44f",
      "#ec7014"
    )
    selection <- input$BeerSelection
    
    idxs <- beer_nn %>% filter(Brand %in% selection, Neighbor_Rk <= 5) %>% pull(Neighbor_Idx)
    neighbor_points <- beer_full_df %>%
      filter(Brand != selection) %>%
      slice(idxs) %>%
      pull(Brand)
    
    
    beer_full_df$Recommendation <- if_else(beer_full_df$Brand %in% neighbor_points,"Recommended","Other")
    
    ### --- UMAP Plot--- ###
    
    gg_umap <- ggplot(data = beer_full_df,aes(x = UMAP_X,
                                              y = UMAP_Y,
                                              fill = Recommendation,
                                              color = Recommendation,
                                              size = Recommendation,
                                              text = paste0(Brand,"<br>",Brand_Style,"<br>","ABV: ",ABV))) +
      geom_jitter(width = 1.2,
                  height = 1.2,
                  alpha = .6,
                  show.legend = TRUE) +
      labs(title = "UMAP Respresentation of Beer Data",
           x = "UMAP 1",
           y = "UMAP 2") +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      scale_size_manual(values = c(1.75,2.75)) +
      theme(plot.title = element_markdown(size = 14),
            plot.subtitle = element_markdown(size = 10),
            plot.caption = element_markdown(),
            legend.position = "bottom",
            legend.title = element_markdown(size = 10),
            axis.title = element_markdown(size = 10),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background  = element_rect(fill = "white"),
            panel.grid.major = element_line(colour="#F0F3F4", size=0.25),
            axis.line = element_line(color = "#d9d9d9",size = 1.5))
    
    ggplotly(gg_umap, tooltip = "text") %>%
      config(displayModeBar = F) %>%
      layout(title = list(text = paste0("UMAP Coordinates",
                                        "<br>",
                                        "<sup>",
                                        "See methodology tab",
                                        "</sup>")),
             legend = list(orientation = "h",
                           y = 15, x = 0.5))
  })
    
  output$Neighbor_Table <- function() {
    
    selection <- input$BeerSelection
    idxs <- beer_nn %>% filter(Brand %in% selection, Neighbor_Rk <= 5) %>% pull(Neighbor_Idx)
    neighbor_points <- beer_full_df %>%
      filter(Brand != selection) %>%
      slice(idxs) %>%
      select(Brand, Brand_Style, ABV, Calories)
    neighbor_points <- neighbor_points %>%
      mutate(ABV = color_tile("white","#df8d03")(ABV),
             Calories = color_bar("#fae96f")(Calories))
    kable(neighbor_points, escape = F) %>% 
      kable_styling(full_width = FALSE, position = "left",
                    bootstrap_options = c("hover")) %>% 
      row_spec(1:nrow(neighbor_points), color = "black") %>%
      column_spec(3:4,width = "1cm") %>% 
      add_header_above(c(" "= 2, "Nutrition" = 2))
  }
 
}
    
## To be copied in the UI
# mod_UMAP_Scatter_ui("UMAP_Scatter_ui_1")
    
## To be copied in the server
# callModule(mod_UMAP_Scatter_server, "UMAP_Scatter_ui_1")
 
