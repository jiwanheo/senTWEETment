#' Bottom module

#' @description A shiny Module, that contains the last step of the app, namely
#' the resulting analysis, and a download button. The analysis should contain
#' the information about the data & method feeding in, and of course, the
#' sentiment score.

#' @param id The Module namespace
#' @rdname mod_bot
#' @importFrom shiny NS tagList textOutput plotOutput htmlOutput
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
        htmlOutput(ns("output_summary")),
        plotOutput(ns("output_plot"))
      ),
      col_12(
        DTOutput(ns("output_data")),
        DTOutput(ns("output_data2"))
      )
    )
  )
}

#' @rdname mod_bot
#' @param ta TweetAnalysis object, to hold analysis process in R6.
#' @importFrom shiny moduleServer renderText renderPlot
#' @importFrom DT datatable renderDT formatStyle styleInterval
mod_bot_server <- function(id, ta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$output_summary <- renderText({

      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$overall_scores))

      ta$print_analysis()
    })

    output$output_plot <- renderPlot({

      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$word_plot))

      ta$analysis_result$word_plot

    })

    output$output_data <- renderDT({

      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$all_tweets_scored))

      datatable(ta$analysis_result$all_tweets_scored,
                class = "hover row-border",
                escape = FALSE,
                options = list(scrollX = TRUE,
                               pageLength = 5)) %>%
        formatStyle(
          'Sentiment',
          backgroundColor = styleInterval(c(-0.5, 0.5), c('hsla(0, 100%, 78%, 0.7)',
                                                          'hsla(0, 0%, 100%, 1)',
                                                          'hsla(137, 66%, 72%, 0.63)'))
        )
    })

    output$output_data2 <- renderDT({

      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$all_words_scored))

      datatable(ta$analysis_result$all_words_scored,
                class = "hover row-border",
                escape = FALSE,
                options = list(scrollX = TRUE,
                               pageLength = 5)) %>%
        formatStyle(
          'Sentiment',
          backgroundColor = styleInterval(c(-0.5, 0.5), c('hsla(0, 100%, 78%, 0.7)',
                                                          'hsla(0, 0%, 100%, 1)',
                                                          'hsla(137, 66%, 72%, 0.63)'))
        )
    })
  })
}

## To be copied in the UI
# mod_bot_ui("bot_1")

## To be copied in the server
# mod_bot_server("bot_1")
