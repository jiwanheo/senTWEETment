#' Bottom module

#' A shiny Module, that contains the last step of the app, namely
#' the resulting analysis, and a download button. The analysis output contains
#' the information about the data & method feeding in, and of course, the
#' sentiment score.

#' @param id The Module namespace
#' @rdname mod_bot
#' @importFrom shiny NS tagList textOutput plotOutput htmlOutput
#' @importFrom shinydashboard box infoBoxOutput
#' @importFrom DT DTOutput
mod_bot_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Step 3: Analysis Result",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,

      col_12(
        tags$h3("Summary"),
        infoBoxOutput(ns("n_tweets")),
        infoBoxOutput(ns("overall_sentiment")),
        infoBoxOutput(ns("avg_sentiment"))
      ),
      col_12(
        tags$h3("Top 10 most impactful words"),
        plotOutput(ns("output_plot"))
      ),
      col_12(
        tags$h3("Sentiment by tweet"),
        DTOutput(ns("output_data")),
      ),
      col_12(
        tags$h3("Sentiment by word"),
        DTOutput(ns("output_data2"))
      )
    ) %>% tagAppendAttributes(class = "main-step")
  )
}

#' @rdname mod_bot
#' @param ta TweetAnalysis object, to hold analysis process in R6.
#' @importFrom shiny moduleServer renderText renderPlot
#' @importFrom shinydashboard renderInfoBox infoBox
#' @importFrom DT datatable renderDT formatStyle styleInterval
mod_bot_server <- function(id, ta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$n_tweets <- renderInfoBox({
      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$overall_scores$sentiment_n))

      infoBox(
        "# Tweets",
        ta$analysis_result$overall_scores$sentiment_n,
        icon = shiny::icon("twitter")
      )
    })

    output$overall_sentiment <- renderInfoBox({
      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$overall_scores$sentiment_sum))

      icon <- ifelse(ta$analysis_result$overall_scores$sentiment_sum < 0,
                     "frown",
                     "smile")
      clr <- ifelse(ta$analysis_result$overall_scores$sentiment_sum < 0,
                    "red",
                    "green")

      infoBox(
        "Total Sentiment",
        ta$analysis_result$overall_scores$sentiment_sum,
        icon = shiny::icon(icon),
        color = clr
      )
    })

    output$avg_sentiment <- renderInfoBox({
      watch("analyze-tweets")
      req(!is.null(ta$analysis_result$overall_scores$sentiment_avg))

      icon <- ifelse(ta$analysis_result$overall_scores$sentiment_avg < 0,
                     "frown",
                     "smile")

      clr <- ifelse(ta$analysis_result$overall_scores$sentiment_avg < 0,
                    "red",
                    "green")

      infoBox(
        "Avg. Sentiment",
        ta$analysis_result$overall_scores$sentiment_avg,
        icon = shiny::icon(icon),
        color = clr
      )
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
