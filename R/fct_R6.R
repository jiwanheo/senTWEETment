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
#' @section Public fields:
#' * `data`: Tweets in tibble.
TweetAnalysis <- R6Class(
  "TweetAnalysis",
  public = list(

    #' @field data Pulled tweets. This is subject to change, every time a set
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
    #' @param negation_word Negation word
    add_negation_word = function(negation_word) {

      negation_word <- tolower(negation_word)

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
    #' @param negation_word Negation word
    remove_negation_word = function(negation_word) {

      negation_word <- tolower(negation_word)

      if(negation_word %in% self$negation_words) {
        self$negation_words <- self$negation_words[self$negation_words != negation_word]
      }
      else {
        warning("Not a registered negation word!")
      }

      invisible(self)
    },

    # Stop words--------------------------------------------------------------

    #' @field stop_words  Stop words that are irrelevant from analysis.
    #' Dataset from tidytext.
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
    #' Conduct sentiment analysis on self$data, using self$lexicons,
    #' self$stop_words and self$negation_words.
    analyze = function() {
      if(is.null(self$data)) {

        stop("No tweets pulled yet!")

        invisible(self)
      }
      else {
        analysis_outputs <- tokenize_tweets(
          tweets = self$data,
          lexicons = self$lexicons,
          stop_words = self$stop_words,
          negation_words = self$negation_words,
          adjust_negation = self$adjust_negation
        )

        return(analysis_outputs)

        # or I can take the list of outputs, and plug them into a R6 field!
      }
    },

    #' @field analysis_result A list of two tibbles and a ggplot plot, that
    #' contain the result of text analysis, performed by self$analyze()
    analysis_result = NULL,

    # Initialize ---------------------------------------------------------------

    #' @description
    #' Pass in the stop_words internal data.
    #'
    #' @param stop_words stop_words internal data that is available in
    #' R/sysdata.rda. I don't know why I need to initialize this, and
    #' simply doing stop_words = stop_words don't work?
    #'
    #' @param lexicons A list of tibbles containing lexicons.
    #'
    initialize = function(stop_words, lexicons) {
      stopifnot(is.data.frame(stop_words))
      stopifnot(is.list(lexicons) & all(unlist(lapply(lexicons, is.data.frame))))

      self$stop_words <- stop_words
      self$lexicons <- lexicons
    }
  )
)


