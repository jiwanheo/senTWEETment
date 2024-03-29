% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_twitter.R
\name{pull_tweets}
\alias{pull_tweets}
\title{Pull tweets}
\usage{
pull_tweets(
  q = "",
  user = "",
  location = "",
  n = 10,
  type = "Recent",
  include_rts = TRUE
)
}
\arguments{
\item{q}{Search text, or hashtag.}

\item{user}{If provided, the user's timeline will be pulled}

\item{location}{If provided on its own, tweets from the area will be pulled.
It can be used with \code{q} argument, but not \code{user}}

\item{n}{Number of tweets to pull. This number is not guaranteed}

\item{type}{Methods to order tweets by}

\item{include_rts}{Whether or not to include retweets.}
}
\description{
Pull tweets using the parameters passed by the user.
}
\details{
Users can pull tweets in 1 of 4 ways.
\itemize{
\item{q: } {Just using search word}
\item{user:} {Just using user name}
\item{location:} {Just using location}
\item{q + location:} {Quering search word, from a location}
}
}
