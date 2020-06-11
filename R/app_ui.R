#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinythemes
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      #Theme
      theme = shinytheme("lumen"),
      navbarPage(title = "Beer Me",windowTitle = "Beer Me",
                 tabPanel("Beer Comparisons",
                          h2(id="page_title", "Beer Me"),
                          p(id = "page_description", "Here's how it works: you choose one of your favorite beers, and the algorithm
                            will give you 5 recommendations for similar beers. Some beers might be very similar, but others might
                            surprise you. The data used in the analysis includes all of the nutritional information (calories, ABV, sodium,
                            etc.) as well as the ingredients used in the beer."),
                          br(),
                          p("The scatterplot is a representation of all 220 beers in a low-dimensional space using a technique 
                          called Uniform Manifold Approximation and Projection (UMAP). The Comparisons plot shows the 
                          5 nearest neighbors from the UMAP algorithm. For information on the technical details of UMAP, please see 
                          the methodology tab."),
                          mod_UMAP_Scatter_ui("UMAP_Scatter_ui_1"),
                          ),
                 tabPanel("Beer Data"),
                 tabPanel("Methodology")
                )
  )
)
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'MCBeerRecommender'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

