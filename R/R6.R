#' R6 class of tweet analyzing process
#'
#' R6 object to encapsulate the reading/cleaning/export of tweets
#'
#' @importFrom R6 R6Class
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom shinyalert shinyalert
#'
TweetAnalysis <- R6Class(
  "TweetAnalysis",
  public = list(

    #' @field data Pulled tweets in tibble. This is subject to change, every time a set
    #' of tweets are pulled.
    data = NULL,

    #' @field lexicons A list of tibbles containing lexicons. It is NULL by
    #' default, but is given the list at initialization
    lexicons = NULL,

    # Negation words------------------------------------------------------------

    #' @field adjust_negation If "yes", do adjust bi-gram adjustment.
    adjust_negation = "yes",

    #' @field negation_words  Negation words used for bi-gram adjustments.
    negation_words = c("not", "no", "never", "without",
                       "didn't", "didnt", "don't", "dont",
                       "doesn't", "doesnt", "isn't", "isnt",
                       "wasn't", "wasnt", "shouldn't", "shouldnt",
                       "couldn't", "couldnt", "won't", "wont",
                       "can't", "cant"),

    # Alot of the ones I added are already a word word. (Let's think
    # about if I want to remove them from the word words or not.)

    #' @description
    #' Add word to negation_words (lower case). Throw a warning if the word
    #' already exists.
    #' @param word Negation word
    add_negation_word = function(word) {

      negation_word <- tolower(word)

      if(!negation_word %in% self$negation_words) {
        self$negation_words <- c(self$negation_words, negation_word)
      }
      else {
        warning("Already a negation word!")
      }

      invisible(self)
    },

    #' @description
    #' Remove word from negation_words (lower case). Throw a warning if the
    #' word doesn't exists.
    #' @param word Negation word
    remove_negation_word = function(word) {

      negation_word <- tolower(word)

      if(negation_word %in% self$negation_words) {
        self$negation_words <- self$negation_words[self$negation_words != negation_word]
      }
      else {
        warning("Not a registered negation word!")
      }

      invisible(self)
    },

    # Stop words--------------------------------------------------------------

    #' @field stop_words  A tibble of stop words that are irrelevant from
    #' analysis. Dataset from tidytext is passed, during initialize.
    stop_words = NULL,

    #' @description
    #' Add word from stop_words (lower case). Throw a warning if the word
    #' already exists.
    #' @param word Stop word
    add_stop_word = function(word) {

      word <- tolower(word)

      if(!word %in% self$stop_words$word) {
        self$stop_words <- rbind(self$stop_words,
                                   tibble(word = c(word),
                                          lexicon = c("custom")))
      }
      else {
        warning("Already a stop word!")
      }

      invisible(self)
    },

    #' @description
    #' Remove word from stop_words (lower case). Throw a warning if the word
    #' doesn't exists.
    #' @param word Stop word
    remove_stop_word = function(word) {

      word <- tolower(word)

      if(word %in% self$stop_words$word) {
        self$stop_words <- self$stop_words %>%
          filter(.data$word != word)
      }
      else {
        warning("Not a registered stop word!")
      }

      invisible(self)
    },

    # Conduct analysis----------------------------------------------------------

    #' @description
    #' Conduct sentiment analysis. It calls `conduct_analysis` using arguments:
    #' \itemize{
    #'   \item{self$data}
    #'   \item{self$lexicons}
    #'   \item{self$stop_words}
    #'   \item{self$negation_words}
    #'   \item{self$adjust_negation}
    #' }
    #' Which returns a list of tibbles. This list is fed to
    #' `produce_analysis_output` to produce final outputs, and are saved into
    #' `self$analysis_result`
    analyze = function() {
      if(is.null(self$data)) {
        stop("No tweets pulled yet!")
        invisible(self)
      }
      else if(is.null(self$lexicons)) {
        stop("No lexicons selected!")
        invisible(self)
      }
      else {
        sentiment_dfs <- conduct_analysis(
          tweets = self$data,
          lexicons = self$lexicons,
          stop_words = self$stop_words,
          negation_words = self$negation_words,
          adjust_negation = self$adjust_negation
        )

        produce_analysis_output(sentiment_dfs)
      }
    },

    #' @field analysis_result A list of two tibbles and a ggplot plot, that
    #' contain the result of text analysis, performed by self$analyze()
    analysis_result = NULL,

    #' @description
    #' Prints out key information about the analysis, as a vector of characters
    #' including html br tag.
    #' \itemize{
    #'   \item{# of tweets}
    #'   \item{Overall sentiment}
    #'   \item{Average sentiment per tweet}
    #' }
    print_analysis = function() {

      total_tweets <- paste("# of tweets:", nrow(self$data))
      sent_sum <- paste("Overall sentiment:", self$analysis_result$overall_scores$sentiment_sum)
      sent_avg <- paste("Average sentiment per tweet:", self$analysis_result$overall_scores$sentiment_avg)

      paste0(total_tweets, '<br>', sent_sum, '<br>', sent_avg)
    },
    # Initialize ---------------------------------------------------------------

    #' @description
    #' Generate a new class. It requires the stop_words internal data.
    #'
    #' @param stop_words stop_words internal data.
    initialize = function(stop_words) {
      stopifnot(is.data.frame(stop_words))

      self$stop_words <- stop_words
    }
  )
)


