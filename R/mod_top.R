#' Top module
#'
#' @description A shiny Module that contains the first step of the app,
#' namely the various ways to pull in tweets. Users must specifty 1 of 4 ways
#' to filter tweets, and hit "Pull Tweets!" button, which will pull in tweets
#' and display as a table. Once table is loaded, the users can move on to
#' the second step.
#'
#' @param id The Module namespace
#' @rdname mod_top
#' @importFrom shiny NS tagList textInput actionButton tableOutput
#' @importFrom shinydashboard box
mod_top_ui <- function(id){
  ns <- NS(id)

  tagList(
    box(
      width = 12,
      title = "Step 1: Pull in Tweets",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,

      # col_3 & col_9 left/right setup in the box
      col_3(
        class = "box-content box-content-left",
        col_12(
          textInput(
            ns("hashtag"),
            "Hashtag:",
            value = ""
          )
        ),
        col_12(
          textInput(
            ns("location"),
            "Location:",
            value = ""
          )
        ),
        col_12(
          textInput(
            ns("user"),
            "User:",
            value = ""
          )
        ),
        col_12(
          textInput(
            ns("tweet_id"),
            "Tweet ID:",
            value = ""
          )
        ),
        col_12(
          actionButton(
            ns("pull_tweets"),
            "Pull Tweets!"
          )
        )
      ),

      col_9(
        class = "box-content box-content-right",
        tableOutput(ns("table"))
      )
    )
  )
}


#' @rdname mod_top
#' @importFrom shiny renderTable
#' @importFrom shinipsum random_table
mod_top_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table <- renderTable({
      random_table(10, 5)
    }, caption = "List of Tweets to analyze")
  })
}

## To be copied in the UI
# mod_top_ui("top_1")

## To be copied in the server
# mod_top_server("top_1")
