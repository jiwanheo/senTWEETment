#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @export
#'
#' @importFrom shiny NS tagList
mod_main_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container",
      tags$div(
        class = "row",
        mod_header_ui(ns("header_1"))
      ),
      tags$div(
        class = "row",
        mod_top_ui(ns("top_1"))
      ),
      tags$div(
        class = "row",
        mod_mid_ui(ns("mid_1"))
      ),
      tags$div(
        class = "row",
        mod_bot_ui(ns("bot_1"))
      )
    )
  )
}

#' main Server Functions
#'
#' @noRd
#' @export
#'
#' @importFrom shiny showModal reactiveVal observeEvent
mod_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Retrieve Twitter API credentials from .Renviron
    creds <- get_creds()
    # Save "creds" in reactiveVal, to fire off reactive triggers, but not actually use it.
    creds_holder <- reactiveVal()

    # This event is triggered when the above reactiveVal changes (when creds are retrieved)
    # It is listened in `mod_creds_modal`, so it can un-grey inputs.
    init("missing_creds")

    observeEvent(creds_holder, {
      trigger("missing_creds")
    })
    # Update reactiveVal
    creds_holder(creds)


    # if creds not found (get_creds returns a character vector of
    # missing cred names, rather than a named list of creds), then launch modalBox
    # if found, everything's good.
    if(!is.list(creds)) {
      showModal(
        mod_creds_modal_ui(ns("creds_modal_1"))
      )
    }

    mod_creds_modal_server("creds_modal_1", to_enable = creds)
    mod_header_server("header_1")
    mod_top_server("top_1")
    mod_mid_server("mid_1")
    mod_bot_server("bot_1")

  })
}

## To be copied in the UI
# mod_mod_main_ui("mod_main_1")

## To be copied in the server
# mod_mod_main_server("mod_main_1")
