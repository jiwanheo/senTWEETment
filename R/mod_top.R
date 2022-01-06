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
#' @importFrom shiny NS fluidRow tagList textInput actionButton tableOutput numericInput radioButtons
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
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

      # col 12 / (6 + 6) / 12 setup
      fluidRow(
        class = "box-content box-content-left",
        col_12(
          tags$p("Use 1 of hashtag, location or user to look up tweets, with the exception of hashtag+location working with each other.")
        ),
        col_6(
          col_12(
            textInput(
              ns("q"),
              "Hashtag or text:",
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
            actionButton(
              ns("pull_tweets"),
              "Pull Tweets!"
            )
          )
        ),
        col_6(
          col_12(
            numericInput(
              ns("n_tweets"),
              "Number of tweets: (not guaranteed)",
              value = 10
            )
          ),
          col_12(
            radioButtons(
              ns("include_rts"),
              "Include Retweets?",
              choices = c("Yes", "No"),
              selected = "No"
            )
          ),
          col_12(
            radioButtons(
              ns("pull_by"),
              "Pull By",
              choices = c("Recent", "Popular", "Mixed"),
              selected = "Recent"
            )
          )
        )
      ),

      fluidRow(
        class = "box-content box-content-right",
        col_12(
          tags$h3("Tweets"),
          DTOutput(ns("table"))
        )
      )
    )
  )
}


#' @rdname mod_top
#' @importFrom shiny renderTable observeEvent updateTextInput req
#' @importFrom DT renderDT datatable
#' @importFrom shinyalert shinyalert
mod_top_server <- function(id){






  #  _______ ____  _____   ____
  #  |__  __/ __ \|  __ \ / __ \
  #    | | | |  | | |  | | |  | |
  #    | | | |  | | |  | | |  | |
  #    | | | |__| | |__| | |__| |
  #    |_|  \____/|_____/ \____/

  # Make pretty the "Tweets" table (actually make it useful)
    # All information is extracted!!
    # Now just have to make it pretty
      # Round profile image
      # DT header names
      # Nice background colour
      # I actually have to set a min-width to the whole thing, so the col_12s don't get shifted around.
  # Potentially in another box?

  # Potentially grey out/disable "include_rts" & "pull_by" when looking up user
  # Spinner for waiting
  # Fail graciously when Twitter API credential fails





  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$pull_tweets, {
      tweets <- tryCatch(
        pull_tweets(q = input$q,
                    user = input$user,
                    location = input$location,
                    n = input$n_tweets,
                    type = input$pull_by,
                    include_rts = input$include_rts
        ),
        error = function(e) {

          shinyalert(
            html = TRUE,
            title = e$message,
            type = "error",
            inputId = "shinyalert_error1"
          )
        }
      )

      output$table <- renderDT({
        req(is.data.frame(tweets))
        datatable(tweets,
                  escape = FALSE,
                  options = list(scrollX = TRUE))
      })
    })

  })
}

## To be copied in the UI
# mod_top_ui("top_1")

## To be copied in the server
# mod_top_server("top_1")
