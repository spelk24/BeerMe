#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinythemes
#' @noRd
# Run the application


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    #golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      #Theme
      theme = shinytheme("lumen"),
      navbarPage(title = "Beer Me",windowTitle = "Beer Me",
                 tabPanel("Beer Comparisons",
                          tags$div(
                            h2(class="page_title", "Beer Me"),
                            h5(class = "page_subtitle", "All data used in this application is from:",a("molsoncoors.com",href="https://www.molsoncoors.com/av?url=https://www.molsoncoors.com/"))
                          ),
                          p(id = "page_description", "Here's how it works: you choose one of your favorite beers, and the algorithm
                            will give you 5 recommendations for similar beers. Some beers might be very similar (Summer Shandy and Grapefruit Shandy), 
                            but others might surprise you. The data used in the analysis includes all of the nutritional information (calories, ABV, sodium,
                            etc.) as well as the ingredients used in the beer."),
                          br(),
                          p("The table shows the 5 nearest neighbors for each beer - which are the top 5 recommendations.
                          The scatterplot is a representation of all 220 beers in a 2-dimensional space using a technique 
                          called Uniform Manifold Approximation and Projection (UMAP). For information on the technical details of UMAP, please see 
                          the methodology tab."),
                          mod_UMAP_Scatter_ui("UMAP_Scatter_ui_1"),
                          ),
                 tabPanel("Methodology",includeMarkdown("Methodology/Methodology.md"))
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
#golem_add_external_resources <- function(){
#  
#  add_resource_path(
#    'www', app_sys('app/www')
#  )
# 
#  tags$head(
#    favicon(),
#    bundle_resources(
#      path = app_sys('app/www'),
#      app_title = 'BeerMe'
#    )
#    # Add here other external resources
#    # for example, you can add shinyalert::useShinyalert() 
#  )
#}

