#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny fluidPage
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      mod_main_ui("main_1")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyWidgets useShinydashboard
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter useWaiter
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(ext = 'ico'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'senTWEETment'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    useShinydashboard(),
    useShinyjs(),
    useWaiter()
  )
}

