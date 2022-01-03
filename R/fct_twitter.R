#' twitter
#'
#' @title Retrieve Twiter API keys
#' @description Retrieve Twitter API keys either from .Renviron file, or users
#' can manually pass them into this function. If all 4 credentials are found,
#' this function returns a named list containing the keys. If any are missing,
#' this function returns a character vector containing the names of the missing
#' keys.
#'

get_creds <- function(
  api_key             = NULL,
  api_key_secret      = NULL,
  access_token        = NULL,
  access_token_secret = NULL
) {

  # If creds are not manually passed, get them from .Renviron
  # if manually passed, just use them
  if(any(is.null(c(api_key,
                   api_key_secret,
                   access_token,
                   access_token_secret)))) {

    api_key             <- Sys.getenv("TWITTER_API_KEY")
    api_key_secret      <- Sys.getenv("TWITTER_API_KEY_SECRET")
    access_token        <- Sys.getenv("TWITTER_ACCESS_TOKEN")
    access_token_secret <- Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")

  }

  creds <- list(api_key             = api_key,
                api_key_secret      = api_key_secret,
                access_token        = access_token,
                access_token_secret = access_token_secret)

  # If creds aren't completely filled out, return a character vector with
  # missing cred names. If found everything, return the named list containing creds.
  if(any(creds == "")) {
    unfilled_creds <- names(creds[creds == ""])
    return(unfilled_creds)
  }
  else {
    return(creds)
  }
}

