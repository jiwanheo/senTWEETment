#' Connect to Twitter API
#'
#' Connect to Twitter API using the credentials provided.
#'
#' @param bearer_token User specific bearer token
#' @importFrom rtweet rtweet_app auth_save auth_as
#' @importFrom shinyalert shinyalert
connect_to_api <- function(bearer_token) {
  # throw an error if no creds are provided
  stopifnot(!is.null(bearer_token))

  auth <- rtweet_app(bearer_token)
  auth_save(auth, "my-twitter-app")
  auth_as("my-twitter-app")

  shinyalert("Connected!", "Start analyzing tweets!", type = "success", inputId = "shinyalert_connected_1")
}

#' Pull tweets
#'
#' Pull tweets using the parameters passed by the user
#'
#' @param q Search text, or hashtag.
#' @param user If provided, the user's timeline will be pulled
#' @param location If provided on its own, tweets from the area will be pulled. It can be used with `q` argument, but not `user`
#' @param n Number of tweets to pull. This number is not guaranteed
#' @param type Methods to order tweets by
#' @param include_rts Whether or not to include retweets.
#' @importFrom rtweet search_tweets get_timeline lookup_coords
pull_tweets <- function(q, user, location = NULL,
                        n, type, include_rts) {
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
  else if (q != ""){
    tweets <- search_tweets(
      q = q,
      n = n,
      type = type,
      include_rts = include_rts
    )
  }
  else {
    tweets <- search_tweets(
      q = q,
      n = n,
      type = type,
      include_rts = include_rts,
      geocode = lookup_coords(location)
    )
  }

  process_tweets(tweets)
}

#' @param tweets A data.frame containing tweets.
#' @importFrom dplyr transmute mutate
process_tweets <- function(tweets) {
  processed <- tweets %>%
    mutate(
      user_name = attr(., "users")$name,
      screen_name = attr(., "users")$screen_name,
      profile_image = attr(., "users")$profile_image_url_https,
      user_url = paste0("https://twitter.com/", screen_name),
      tweet_url = paste0(user_url, "/status/", id_str),
      created_at =
    ) %>%
    transmute(
      profile_image = sprintf('<a href="%s" target=_blank><img class="profile-table-img" src=%s></img></a>',
                              user_url,
                              profile_image),
      user_name,

      # format date-time
      created_at = paste(
        as.character(strptime(gsub("\\+0000\\s", "", created_at),
                              format = "%a %b %d %H:%M:%S %Y")),
        "UTC"),
      text = sprintf('<a href="%s">%s</a>',
                     tweet_url,
                     text)
    )

  names(processed) <- c("Picture", "User", "Date", "Text")

  processed

}
