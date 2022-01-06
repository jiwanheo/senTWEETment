#' Connect to Twitter API
#'
#' Connect to Twitter API using the credentials provided.
#'
#' @param  bearer_token User specific bearer token
#' @importFrom rtweet rtweet_app auth_save auth_as
#' @importFrom shinyalert shinyalert
connect_to_api <- function(bearer_token) {
  # throw an error if no creds are provided
  stopifnot(!is.null(bearer_token))

  auth <- rtweet_app(bearer_token)
  auth_save(auth, "my-twitter-app")
  auth_as("my-twitter-app")

  shinyalert("Connected!", "Start analyzing tweets!", type = "success")
}

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
    get_timeline(
      user = user,
      n = n
    )
  }
  else if (q != ""){
    search_tweets(
      q = q,
      n = n,
      type = type,
      include_rts = include_rts
    )
  }
  else {
    search_tweets(
      q = q,
      n = n,
      type = type,
      include_rts = include_rts,
      geocode = lookup_coords(location)
    )
  }
}
