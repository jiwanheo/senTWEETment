#' Bottom module

#' @description A shiny Module, that contains the last step of the app, namely
#' the resulting analysis, and a download button. The analysis should contain
#' the information about the data & method feeding in, and of course, the
#' sentiment score.

#' @param id The Module namespace
#' @rdname mod_bot
#' @importFrom shiny NS tagList textOutput plotOutput downloadButton
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
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
        tags$h3("Analysis result"),
        textOutput(ns("sentiment_score")),
        plotOutput(ns("word_score"))
      ),
      col_12(
        tags$h3("Methodologies & input data"),
        textOutput(ns("methodologies")),
        DTOutput(ns("input_data"))
      ),
      col_12(
        downloadButton(ns("download_analysis"), "Download Analysis (pdf)"),
        downloadButton(ns("download_tweets"), "Download tweets (csv)")
      )
    )
  )
}

#' @rdname mod_bot
#' @importFrom shiny moduleServer renderText renderPlot
#' @importFrom DT datatable renderDT
mod_bot_server <- function(id, ta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$sentiment_score <- renderText({

      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$overall_score))

      score <- ta$analysis_result$overall_score
      score <- ifelse(score < 0, paste0("-", score), paste0("+", score))

      paste("The overall sentiment is", score)
    })

    output$word_score <- renderPlot({

      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$word_plot))

      ta$analysis_result$word_plot

    })

    output$input_data <- renderDT({

      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$all_tweets_scored))

      datatable(ta$analysis_result$all_tweets_scored,
                class = "hover row-border",
                escape = FALSE,
                options = list(scrollX = TRUE,
                               pageLength = 5),
                caption = "Tweets that were analyzed")
    })

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
