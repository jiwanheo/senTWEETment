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
