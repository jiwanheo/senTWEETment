#' creds_modal Module
#'
#' A shiny Module containing a popup box that asks users for
#' Twitter Dev API bearer token. Once user inputs the token, it authenticates,
#' saves as .rds

#' @rdname mod_creds_modal
#' @param id Module Id.
#' @importFrom shiny moduleServer tagList HTML textInput observeEvent
#' @importFrom shinyalert shinyalert
mod_creds_modal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Inputs to retrieve Bearer Token
    shinyalert(
      html = TRUE,
      title = "This app requires a Twitter API Bearer Token!",
      text = tagList(
        HTML('Click <a href="https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html">here</a> for a how-to.<br>'),
        textInput(
          ns("bearer_token"),
          "",
          placeholder = "BEARER_TOKEN"
        )
      ),
      inputId = "shinyalert_input1"
    )

    observeEvent(input$shinyalert_input1, {
      tryCatch({
        connect_to_api(input$bearer_token, saved = FALSE)
        shinyalert("Connected!", "Start analyzing tweets!", type = "success", inputId = "shinyalert_connected_1")
      },
      error = function(e) {
        shinyalert(e$message,
                   type = "error",
                   inputId = "shinyalert_failed_1")
      })
    })
  })
}

## To be copied in the UI
# mod_creds_modal_ui("creds_modal_1")

## To be copied in the server
# mod_creds_modal_server("creds_modal_1")
