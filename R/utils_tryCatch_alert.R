#' tryCatch with shinyalert messages
#'
#' @description A tryCatch with output wrapped in shinyalert.
#' @param expr Expression to try. In our case, add/remove words from R6 fields.
#' @param input_id the input ID of the text. (negation_word or stop_word)
#' @param type A string, that makes the shinyalert ID. (add or remove)
#' @importFrom shiny updateTextInput
#' @importFrom shinyalert shinyalert

tryCatch_alert <- function(expr, input_id, type) {

  alert_id <- paste0("shinyalert_", type, "_", input_id)

  tryCatch({
    expr
    shinyalert(
      title = "Done!",
      type = "success",
      inputId = alert_id
    )
  },
  warning = function(w) {
    shinyalert(
      title = w$message,
      type = "error",
      inputId = alert_id
    )
  },
  finally = {
    updateTextInput(inputId = input_id,
                    value = "")
  })

}
