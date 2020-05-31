#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_UMAP_Scatter_server, "UMAP_Scatter_ui_1")
  callModule(mod_Nearest_Neighbors_server, "Nearest_Neighbors_ui_1")
}
