#' Connect to Twitter API
#'
#' Connect to Twitter API using the credentials provided.
#'
#' @param bearer_token User specific bearer token
#' @param saved If TRUE, use the existing 'my-twitter-app' authentication. If
#' FALSE, make one, and authenticate with it.
#' @importFrom rtweet rtweet_app auth_save auth_as
#' @importFrom shinyalert shinyalert
connect_to_api <- function(bearer_token, saved = TRUE) {
  # throw an error if no creds are provided
  stopifnot(!is.null(bearer_token))
  stopifnot(bearer_token != "")

  if(saved) {
    auth_as("my-twitter-app")
  }
  else {
    auth <- rtweet_app(bearer_token)
    auth_save(auth, "my-twitter-app")
    auth_as("my-twitter-app")
  }
}

#' Pull tweets
#'
#' Pull tweets using the parameters passed by the user.
#'
#' Users can pull tweets in 1 of 4 ways.
#' \itemize{
#'   \item{q: } {Just using search word}
#'   \item{user:} {Just using user name}
#'   \item{location:} {Just using location}
#'   \item{q + location:} {Quering search word, from a location}
#' }
#'
#' @param q Search text, or hashtag.
#' @param user If provided, the user's timeline will be pulled
#' @param location If provided on its own, tweets from the area will be pulled.
#' It can be used with `q` argument, but not `user`
#' @param n Number of tweets to pull. This number is not guaranteed
#' @param type Methods to order tweets by
#' @param include_rts Whether or not to include retweets.
#' @importFrom rtweet search_tweets get_timeline
pull_tweets <- function(q = "", user = "", location = "",
                        n = 10, type = "Recent", include_rts = TRUE) {
  #input cleaning
  type <- tolower(type)
  include_rts <- ifelse(include_rts == "Yes", TRUE, FALSE)

  # Invalid inputs errors
  if(q == "" && user == "" && location == "") {
    stop("You provided no search parameter!")
  }
  else if(q != "" && user != "") {
    stop("You should only provide one of Search text or User!")
  }
  else if(user != "" && location != "") {
    stop("You can't use both User and Location!")
  }
  # actual tweet pulls
  else if(user != "") {
    tweets <- get_timeline(
      user = user,
      n = n
    )
  }
  else if (location != ""){
    tweets <- search_tweets(
      q = q,
      n = n,
      type = type,
      include_rts = include_rts,
      geocode = lookup_coords_nominatim(location)
    )
  }
  else {
    tweets <- search_tweets(
      q = q,
      n = n,
      type = type,
      include_rts = include_rts
    )
  }

  tweets
}

#' Generate DT-friendly format tweets
#'
#' Process tweets pulled by the user, so that it looks nice on DT.
#'
#' The resulting dataframe contains four columns
#' \itemize{
#'   \item{Picture:} {User picture, with an anchor tag, leading to user profile}
#'   \item{User:} {User name}
#'   \item{Date:} {Date/time in UTC}
#'   \item{Text:} {Text from tweet, wrapped in anchor tag, leading to the tweet}
#' }
#'
#' @param tweets A data.frame containing tweets.
#' @importFrom dplyr transmute mutate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
pretty_tweets <- function(tweets) {

  if(nrow(tweets) == 0) {
    stop("No tweets found!")
  }

  user_name <- attr(tweets, "users")$name
  screen_name <- attr(tweets, "users")$screen_name
  profile_image <- attr(tweets, "users")$profile_image_url_https

  processed <- tweets %>%
    mutate(
      user_name = user_name,
      screen_name = screen_name,
      profile_image = profile_image,
      user_url = paste0("https://twitter.com/", .data$screen_name),
      tweet_url = paste0(.data$user_url, "/status/", .data$id_str),
      created_at =
    ) %>%
    transmute(
      profile_image = sprintf('<a href="%s" target=_blank><img class="profile-table-img" src=%s></img></a>',
                              .data$user_url,
                              .data$profile_image),
      screen_name = paste0("@", .data$screen_name),

      # format date-time
      created_at = paste(
        as.character(strptime(gsub("\\+0000\\s", "", .data$created_at),
                              format = "%a %b %d %H:%M:%S %Y")),
        "UTC"),
      text = sprintf('<a href="%s">%s</a>',
                     .data$tweet_url,
                     .data$text)
    )

  names(processed) <- c("Picture", "User", "Date", "Text")

  processed

}
