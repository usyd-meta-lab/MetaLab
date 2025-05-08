#' Convert JSON responses to long format
#'
#' This function takes a dataframe containing JSON-formatted responses, typically from surveys or experiments,
#' and converts it into a long-format dataframe suitable for analysis.
#'
#' @param data A dataframe containing JSON responses.
#' @param participant_id The name of the column identifying participants.
#' @param response The name of the column containing JSON responses.
#'
#' @return A dataframe in long format, including participant IDs, individual questions, and numeric responses.
#'
#' @examples
#' \dontrun{
#' df_long <- json_to_long(data, participant_id = "id", response = "response_json")
#' }
#'
#' @import dplyr
#' @import purrr
#' @import jsonlite
#' @export
json_to_long <- function(data, participant_id = "participant_id", response = "response") {
  
  df_long <- data %>%
    mutate(row = row_number()) %>%
    group_split(row) %>%
    map_dfr(~ {
      self <- .
      answers <- fromJSON(self[[response]])
      tibble(
        participant_id = self[[participant_id]],
        question = names(answers),
        response = as.numeric(answers)
      )
    })
  
  return(df_long)
}
