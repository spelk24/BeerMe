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
 
  )
}
    
#' UMAP_Scatter Server Function
#'
#' @noRd 
mod_UMAP_Scatter_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_UMAP_Scatter_ui("UMAP_Scatter_ui_1")
    
## To be copied in the server
# callModule(mod_UMAP_Scatter_server, "UMAP_Scatter_ui_1")
 
