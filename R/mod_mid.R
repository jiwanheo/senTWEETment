#' mid UI Function
#' @title   mod_mid_ui and mod_mid_name_server
#' @description A shiny Module that contains the second step of the app,
#' namely the various methods to analyze the tweets. Users can either analyze
#' every word (unigram), or pairs of words (N-gram), using several different
#' dictionaries. If one option is selected, the other should be NULL. Once
#' the method has been determined, the "Analyze!" button executes the analysis.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList actionButton radioButtons
#' @importFrom shinydashboard box
mod_mid_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Step 2: Choose Analyze Methods",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,

      col_12(
        col_6(
          radioButtons(
            ns("unigram-radio"),
            "Option 1: Use Unigram Dictionary",
            choices = c("AFINN", "bing", "nrc"),
            selected = character(0)
          )
        ),
        col_6(
          radioButtons(
            ns("n-gram-radio"),
            "Option 2: Use N-gram Dictionary",
            choices = c("I think there's just 1", "maybe2", "maybe3"),
            selected = character(0)
          )
        )
      ),
      col_12(
        class = "text-center",
        actionButton(ns("analyze"),"Analyze!")
      )
    )
  )
}

#' mid Server Functions
#'
mod_mid_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_mid_ui("mid_1")

## To be copied in the server
# mod_mid_server("mid_1")
