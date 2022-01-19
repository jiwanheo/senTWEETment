# Lexicon related --------------------------------------------------------------
#' Download lexicons
#'
#' Download Lexicons (AFINN, bing, nrc) to the user cache. This
#' function asks the user to explicitly agree the conditions for the data.
#'
#' @param retrieve If TRUE, return a list of 3 lexicons. If FALSE, invisibly
#' return the list. (used for downloading the lexicons initially)
#' @export
#' @importFrom textdata lexicon_afinn lexicon_bing lexicon_nrc

get_lexicons <- function(retrieve = FALSE) {

  afinn <- lexicon_afinn()
  bing <- lexicon_bing()
  nrc <- lexicon_nrc()

  lexicons <- list(
    afinn = afinn,
    bing = bing,
    nrc = nrc
  )

  if(retrieve) {
    lexicons
  }
  else {
    invisible(lexicons)
  }
}

#' Process lexicons
#'
#' Get the lexicons, and process them to word-value format.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr transmute filter
#' @importFrom rlang .data

process_lexicons <- function() {
  lexicons <- get_lexicons(retrieve = TRUE)

  lexicons$bing <- lexicons$bing %>%
    transmute(.data$word,
              value = ifelse(.data$sentiment == "positive", 1, -1))

  lexicons$nrc <- lexicons$nrc %>%
    filter(.data$sentiment %in% c("negative", "positive")) %>%
    transmute(.data$word,
              value = ifelse(.data$sentiment == "positive", 1, -1))

  lexicons
}

# Analysis related--------------------------------------------------------------

#' Conduct Analysis
#'
#' A function to conduct the text sentiment analysis. It first tokenizes the
#' texts with by 1 word, and weed out stop words. This tokenized tibble is
#' `inner_join`'d with the lexicons to get the final score. If `adjust_negation`
#' is "yes", then the bigram negation adjustment will be performed.
#'
#' @param tweets Tweets to analyze. A tibble.
#' @param lexicons Lexicons to use, A named list of tibbles.
#' @param stop_words stop words not relevant to analysis, from
#' TweetAnalysis R6 class. A tibble.
#' @param negation_words Negation words to use from TweetAnalysis R6 class.
#' A tibble.
#' @param adjust_negation Boolean indicating whether or not to use bigram
#' adjustment.
#' @importFrom dplyr mutate select filter inner_join group_by summarize
#' @importFrom magrittr %>%
#' @importFrom tidytext unnest_tokens
#' @importFrom rlang .data
conduct_analysis <- function(tweets,
                             lexicons,
                             stop_words,
                             negation_words,
                             adjust_negation) {

  tweets_by_id <- tweets %>%
    select(-c(.data$Picture, .data$User, .data$Date))

  tokenized <- tweets_by_id %>%
    unnest_tokens(.data$word, .data$Text, token = "tweets")

  # token must not be a stop word
  tokenized_stopped <- tokenized  %>%
    filter_stop_words(stop_words)

  # INNER join with lexicons
  sentimented <- tokenized_stopped %>%
    inner_join(lexicons, by = "word")

  # Bi-gram adjustment ----

  if(adjust_negation == "yes") {
    negated_words <- bigram_adjustment(lexicons, tweets_by_id, negation_words, stop_words)

    # Tally up the unigram sentiments with bigram sentiments.
    sentimented <- sentimented %>%
      rbind(negated_words) %>%
      group_by(.data$ID, .data$word) %>%
      summarize(value = sum(.data$value), .groups = "drop") %>%
      filter(.data$value != 0)
  }

  list(
    tweets = tweets,
    sentimented = sentimented
  )
}

#' Bigram negation adjustment
#'
#' Makes adjustments to assign a negative score to phrases like "I am not happy",
#' that would have gotten a positive score, had it not been adjusted (looking
#' at single word at a time).
#'
#' Traditionally, a single word tokenization results in a single row of
#' "word to sentiment value" per word. This function tokenizes the texts with 2
#' words. Any token that has as the first word, a negative word per
#' `negation_words`, instead gets two rows. One with the full 2-word token, and
#' another row with the original word. The sentiment value of both rows is the
#' sentiment value of the original word multiplied by -1. Then both these rows
#' are appended to the 1-word-tokenized tibble, and are summed at the word/tweet
#' level, canceling out the original word's sentiment, and adding the bigram
#' sentiment. Since we are specifically looking only for the negative words,
#' stop words will exclude negation words.
#' @param lexicons Lexicons to use, A named list of tibbles.
#' @param tweets_by_id Texts of tweets, processed in `produce_analysis_df`
#' @param negation_words Negation words to use from TweetAnalysis R6 class.
#' A character vector.
#' @param stop_words Stop words to use from TweetAnalysis R6 class. A tibble.
#'
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr separate
#' @importFrom tibble tibble
#' @importFrom dplyr filter inner_join mutate
#' @importFrom purrr imap_dfr pmap_dfr
#' @importFrom rlang .data

bigram_adjustment <- function(lexicons, tweets_by_id, negation_words, stop_words) {

  # There are lots of words in stop_words that shouldn't be stop words
  stop_words <- stop_words %>%
    filter(!.data$word %in% negation_words)

  # unigrams, stripped of stop words
  unigrams <- tweets_by_id %>%
    unnest_tokens(.data$word, .data$Text) %>%
    filter_stop_words(stop_words)

  # Make bigrams out of the above unigrams.

  bigrams <- imap_dfr(unigrams$word, function(x, y) {
    curr_row_ID <- unigrams[[y, "ID"]]

    if(y == nrow(unigrams)) {
      # Reached the end of unigrams df
      NULL
    }
    else if(curr_row_ID == unigrams[[y+1, "ID"]]) {
      tibble::tibble(
        ID = curr_row_ID,
        word = paste(unigrams[[y, "word"]], unigrams[[y+1, "word"]])
      )
    }
    else {
      # Last unigram of current sentence. Stop.
      NULL
    }

  }) %>% separate(.data$word, c("word1", "word2"), sep = " ")

  negated_words <- bigrams %>%
    filter(.data$word1 %in% negation_words) %>%
    inner_join(lexicons, by = c("word2" = "word")) %>%
    mutate(word1 = paste(.data$word1, .data$word2)) %>%
    pmap_dfr(function(ID, word1, word2, value){
      data.frame(ID = ID,
                 word = c(word1, word2),
                 value = c(-value, -value))
    })

  negated_words
}

#' Produce the analysis outputs
#'
#' Makes adjustments to assign a negative score to phrases like "I am not happy",
#' that would have gotten a positive score, had it not been adjusted (looking
#' at single word at a time).
#' @param dfs A list of tibbles, passed from `tokenize_tweets`. This list
#' contains the original tweets, as well as tokenized sentiments.
#' @importFrom dplyr group_by summarize left_join mutate arrange slice_max
#' @importFrom ggplot2 ggplot aes geom_col labs
#' @importFrom forcats fct_reorder

produce_analysis_output <- function(dfs) {

  sentiment_by_tweet <- dfs$sentimented %>%
    group_by(.data$ID) %>%
    summarize(Sentiment = sum(.data$value), .groups = "drop")

  all_tweets_scored <- dfs$tweets %>%
    left_join(sentiment_by_tweet, by = "ID") %>%
    mutate(Sentiment = ifelse(is.na(.data$Sentiment), 0, .data$Sentiment))

  all_words_scored <- dfs$tweets %>%
    left_join(dfs$sentimented, by = "ID") %>%
    mutate(value = ifelse(is.na(.data$value), 0, .data$value)) %>%
    select(-c(.data$User, .data$Date, .data$Text))
  names(all_words_scored) <- c("Picture", "ID", "Word", "Sentiment")

  overall_scores <- all_tweets_scored %>%
    summarize(sentiment_n = nrow(all_tweets_scored),
              sentiment_sum = sum(.data$Sentiment),
              sentiment_avg = round(mean(.data$Sentiment), digits = 2)) %>%
    as.list()

  word_plot <- dfs$sentimented %>%
    group_by(.data$word) %>%
    summarize(Sentiment = sum(.data$value)) %>%
    slice_max(abs(.data$Sentiment), n = 10) %>%
    mutate(word = fct_reorder(.data$word, .data$Sentiment)) %>%
    mutate(is_positive = .data$Sentiment > 0) %>%
    ggplot(aes(.data$Sentiment, .data$word, fill = .data$is_positive)) +
    geom_col(show.legend = FALSE) +
    labs(y = "")

  list(
    sentiment_by_tweet = sentiment_by_tweet,
    all_words_scored = all_words_scored,
    all_tweets_scored = all_tweets_scored,
    overall_scores = overall_scores,
    word_plot = word_plot
  )
}

#' Filter stop words from unigrams

#' @param tokenized_unigrams A unigram dataframe.
#' @param stop_words A stop_words dataframe.
#' @importFrom dplyr filter
#' @importFrom rlang .data

filter_stop_words <- function(tokenized_unigrams, stop_words) {

  tokenized_unigrams  %>%
    filter(!grepl("^\\@|^http", .data$word),
           !.data$word %in% stop_words$word,
           !.data$word %in% gsub("'", "", stop_words$word),
           grepl("[a-z]", .data$word))
}
