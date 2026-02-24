#' Add Prolific demographic data to a data frame
#'
#' Fetches participant demographic data (age and sex) from the Prolific API
#' for one or more studies and joins it to an existing data frame by participant
#' ID. The function handles long-format data, multiple study IDs, and cleans
#' Prolific-specific missing value codes (\code{CONSENT_REVOKED} and
#' \code{DATA_EXPIRED}).
#'
#' @param df A data frame containing at least a participant ID column and a
#'   study ID column.
#' @param api_key A character string containing your Prolific API token. Generate
#'   one at \url{https://app.prolific.com} under Settings > API tokens.
#' @param pid_col A character string specifying the name of the participant ID
#'   column in \code{df}. Defaults to \code{"participantId"}.
#' @param sid_col A character string specifying the name of the study ID column
#'   in \code{df}. Defaults to \code{"STUDY_ID"}. Blank and \code{NA} values
#'   are silently skipped.
#'
#' @return A data frame identical to \code{df} with two additional columns:
#'   \describe{
#'     \item{Age}{Participant age as a numeric value. \code{NA} where
#'       unavailable.}
#'     \item{Sex}{Participant sex as a character string (e.g. \code{"Male"},
#'       \code{"Female"}). \code{NA} where unavailable.}
#'   }
#'   Participants with no matching Prolific record will have \code{NA} for both
#'   columns. The original row order and long-format structure of \code{df} are
#'   preserved.
#'
#' @details
#' The function queries \code{GET /api/v1/studies/:id/export/} for each unique
#' study ID found in \code{sid_col}. This endpoint returns a CSV containing
#' one row per submission, from which \code{Age} and \code{Sex} are extracted
#' and joined to \code{df} by participant ID.
#'
#' Prolific export columns accepted as participant ID (in order of preference):
#' \code{"Participant id"}, \code{"participant_id"}, \code{"Participant ID"},
#' \code{"PROLIFIC_PID"}.
#'
#' Where a participant appears in multiple studies, only their first record is
#' used (demographics are profile-level attributes and should be identical
#' across studies).
#'
#' @note
#' Requires a Prolific researcher account with API access. The export endpoint
#' must be accessible to your workspace — contact Prolific support if you
#' receive 403 or 404 errors.
#'
#' @examples
#' \dontrun{
#' api_key <- "your_prolific_api_token"
#'
#' # Basic usage with default column names
#' mydata <- add_prolific_demographics(mydata, api_key)
#'
#' # Custom column names
#' mydata <- add_prolific_demographics(
#'   df      = mydata,
#'   api_key = api_key,
#'   pid_col = "PROLIFIC_PID",
#'   sid_col = "study_id"
#' )
#' }
#'
#' @importFrom httr GET add_headers http_error content status_code
#' @export
add_prolific_demographics <- function(df, api_key,
                                      pid_col = "participantId",
                                      sid_col = "STUDY_ID") {

  base_url    <- "https://api.prolific.com/api/v1"
  auth_header <- add_headers(Authorization = paste("Token", api_key))

  all_ids   <- unique(df[[sid_col]])
  study_ids <- all_ids[!is.na(all_ids) & nchar(trimws(all_ids)) > 0]
  skipped   <- length(all_ids) - length(study_ids)
  if (skipped > 0) message("Skipping ", skipped, " blank/NA study ID(s).")

  message("Fetching export for ", length(study_ids), " study ID(s)...")

  demo_list <- lapply(study_ids, function(sid) {

    message("  Study: ", sid)
    resp <- GET(paste0(base_url, "/studies/", sid, "/export/"), auth_header)

    if (http_error(resp)) {
      warning("Export failed for study ", sid, " (", status_code(resp), "): ",
              content(resp, "text", encoding = "UTF-8"))
      return(NULL)
    }

    raw  <- content(resp, "text", encoding = "UTF-8")
    demo <- read.csv(textConnection(raw), stringsAsFactors = FALSE,
                     check.names = FALSE)
    names(demo) <- trimws(names(demo))

    message("    Retrieved ", nrow(demo), " rows.")

    pid_col_demo <- intersect(
      c("Participant id", "participant_id", "Participant ID", "PROLIFIC_PID"),
      names(demo)
    )[1]

    if (is.na(pid_col_demo)) {
      warning("No participant ID column found for study ", sid,
              ". Columns: ", paste(names(demo), collapse = ", "))
      return(NULL)
    }

    cols_keep <- c(pid_col_demo, intersect(c("Age", "Sex"), names(demo)))
    demo <- demo[, cols_keep, drop = FALSE]
    names(demo)[1] <- pid_col

    demo[demo == "CONSENT_REVOKED"] <- NA
    demo[demo == "DATA_EXPIRED"]    <- NA

    demo
  })

  demo_all <- do.call(rbind, Filter(Negate(is.null), demo_list))

  if (is.null(demo_all) || nrow(demo_all) == 0) {
    warning("No demographic data retrieved. Returning df unchanged.")
    return(df)
  }

  demo_all <- demo_all[!duplicated(demo_all[[pid_col]]), ]
  demo_all$Age <- suppressWarnings(as.numeric(demo_all$Age))

  message("Merging demographics for ", nrow(demo_all), " participant(s).")

  df_out    <- merge(df, demo_all, by = pid_col, all.x = TRUE)
  n_missing <- sum(is.na(df_out$Age))
  if (n_missing > 0) message(n_missing, " participant(s) with no demographic match.")

  return(df_out)
}
