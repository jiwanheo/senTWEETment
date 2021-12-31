#' bot UI Function
#' @title   mod_bot_ui and mod_bot_name_server
#' @description A shiny Module, that contains the last step of the app, namely
#' the resulting analysis, and a download button. The analysis should contain
#' the information about the data & method feeding in, and of course, the
#' sentiment score.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList textOutput plotOutput tableOutput downloadButton
#' @importFrom shinydashboard box
mod_bot_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Step 3: Analysis Result & Export",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,

      col_12(
        h3("Analysis result"),
        textOutput(ns("sentiment_score")),
        plotOutput(ns("word_score"))
      ),
      col_12(
        h3("Methodologies & input data"),
        textOutput(ns("methodologies")),
        tableOutput(ns("input_data"))
      ),
      col_12(
        downloadButton(ns("download_analysis"), "Download Analysis (pdf)"),
        downloadButton(ns("download_tweets"), "Download tweets (csv)")
      )
    )
  )
}

#' bot Server Functions
#'
#' @importFrom shiny renderText renderPlot renderTable downloadHandler
#' @importFrom shinipsum random_ggplot random_table
#' @import ggplot2
mod_bot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$sentiment_score <- renderText({
      "The overall sentiment is +2"
    })

    output$word_score <- renderPlot({
      random_ggplot(type = "col") +
        labs(title = "Sentiment score on each Word") +
        theme_bw()
    })

    output$input_data <- renderTable({
      random_table(10, 5)
    }, caption = "Tweets that were analyzed")

    output$methodologies <- renderText({
      "500 tweets were analyzed using the 'AFINN' Unigram dictionary\n
      The biggest contributor was the word 'happy'"
    })

  })
}

## To be copied in the UI
# mod_bot_ui("bot_1")

## To be copied in the server
# mod_bot_server("bot_1")
