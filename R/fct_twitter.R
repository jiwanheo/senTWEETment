#' twitter
#'
#' @title Retrieve Twiter API keys
#' @description Retrieve Twitter API keys either from .Renviron file, or users
#' can manually pass them into this function.
#'

get_creds <- function(
  api_key             = NULL,
  api_key_secret      = NULL,
  access_token        = NULL,
  access_token_secret = NULL
) {

  # get creds are not manually passed, get them from .Renviron
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

  # If creds aren't completely filled out, return NULL, and say something.
  if(any(creds == "")) {
    unfilled_creds <- names(creds[creds == ""])
    print("Couldn't find following credentials:")
    print(toupper(unfilled_creds))
    print("Please fill them in the modalBox.")

    return(unfilled_creds)
  }
  else {
    return(creds)
  }
}

