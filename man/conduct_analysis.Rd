% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_tidytext.R
\name{conduct_analysis}
\alias{conduct_analysis}
\title{Conduct Analysis}
\usage{
conduct_analysis(tweets, lexicons, stop_words, negation_words, adjust_negation)
}
\arguments{
\item{tweets}{Tweets to analyze. A tibble.}

\item{lexicons}{Lexicons to use, A named list of tibbles.}

\item{stop_words}{stop words not relevant to analysis, from
TweetAnalysis R6 class. A tibble.}

\item{negation_words}{Negation words to use from TweetAnalysis R6 class.
A tibble.}

\item{adjust_negation}{Boolean indicating whether or not to use bigram
adjustment.}
}
\description{
A function to conduct the text sentiment analysis. It first tokenizes the
texts with by 1 word, and weed out stop words. This tokenized tibble is
\code{inner_join}'d with the lexicons to get the final score. If \code{adjust_negation}
is "yes", then the bigram negation adjustment will be performed.
}
