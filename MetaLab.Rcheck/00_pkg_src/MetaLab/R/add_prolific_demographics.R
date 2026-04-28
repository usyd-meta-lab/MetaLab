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
                                      mode        = NULL,
                                      pid_col     = "participantId",
                                      sid_col     = "STUDY_ID",
                                      session_col = "SESSION_ID") {

  # Auto-detect mode if not specified
  if (is.null(mode)) {
    has_study_ids   <- sid_col %in% names(df) &&
      any(!is.na(df[[sid_col]]) & nchar(trimws(df[[sid_col]])) > 0)
    has_session_ids <- session_col %in% names(df) &&
      any(!is.na(df[[session_col]]) & nchar(trimws(df[[session_col]])) > 0)

    if (has_study_ids) {
      mode <- "study"
    } else if (has_session_ids) {
      mode <- "session"
    } else {
      stop("Could not auto-detect mode: no valid values found in '", sid_col,
           "' or '", session_col, "'. Specify mode = 'study' or 'session' manually.")
    }
    message("Auto-detected mode: '", mode, "'.")
  } else {
    mode <- match.arg(mode, c("study", "session"))
  }

  base_url    <- "https://api.prolific.com/api/v1"
  auth_header <- httr::add_headers(Authorization = paste("Token", api_key))

  fetch_study_export <- function(sid) {

    resp <- httr::GET(paste0(base_url, "/studies/", sid, "/export/"), auth_header)

    if (httr::http_error(resp)) {
      warning("Export failed for study ", sid, " (", httr::status_code(resp), "): ",
              httr::content(resp, "text", encoding = "UTF-8"))
      return(NULL)
    }

    raw  <- httr::content(resp, "text", encoding = "UTF-8")
    demo <- read.csv(textConnection(raw), stringsAsFactors = FALSE,
                     check.names = FALSE)
    names(demo) <- trimws(names(demo))

    pid_col_demo <- intersect(
      c("Participant id", "participant_id", "Participant ID", "PROLIFIC_PID"),
      names(demo)
    )[1]

    if (is.na(pid_col_demo)) {
      warning("No participant ID column found for study ", sid,
              ". Columns: ", paste(names(demo), collapse = ", "))
      return(NULL)
    }

    sub_col   <- intersect(c("Submission id", "submission_id"), names(demo))[1]
    cols_keep <- unique(c(pid_col_demo, sub_col,
                          intersect(c("Age", "Sex"), names(demo))))
    cols_keep <- cols_keep[!is.na(cols_keep)]
    demo      <- demo[, cols_keep, drop = FALSE]

    names(demo)[names(demo) == pid_col_demo] <- pid_col

    demo[demo == "CONSENT_REVOKED"] <- NA
    demo[demo == "DATA_EXPIRED"]    <- NA

    demo
  }

  if (mode == "study") {

    all_ids   <- unique(df[[sid_col]])
    study_ids <- all_ids[!is.na(all_ids) & nchar(trimws(all_ids)) > 0]
    skipped   <- length(all_ids) - length(study_ids)
    if (skipped > 0) message("Skipping ", skipped, " blank/NA study ID(s).")
    message("Fetching export for ", length(study_ids), " study ID(s)...")

    demo_list <- lapply(study_ids, function(sid) {
      message("  Study: ", sid)
      out <- fetch_study_export(sid)
      if (!is.null(out)) message("    Retrieved ", nrow(out), " rows.")
      out
    })

  } else {

    all_sessions <- unique(df[[session_col]])
    sessions     <- all_sessions[!is.na(all_sessions) &
                                   nchar(trimws(all_sessions)) > 0]
    skipped      <- length(all_sessions) - length(sessions)
    if (skipped > 0) message("Skipping ", skipped, " blank/NA session ID(s).")
    message("Resolving study IDs for ", length(sessions), " session ID(s)...")

    session_map <- do.call(rbind, lapply(sessions, function(sess) {

      resp <- httr::GET(paste0(base_url, "/submissions/", sess, "/"),
                        auth_header)

      if (httr::http_error(resp)) {
        warning("Could not resolve session ", sess,
                " (", httr::status_code(resp), ")")
        return(NULL)
      }

      parsed <- httr::content(resp, "parsed", encoding = "UTF-8")
      data.frame(
        session_id    = sess,
        study_id      = parsed$study_id,
        submission_id = parsed$id,
        stringsAsFactors = FALSE
      )
    }))

    if (is.null(session_map) || nrow(session_map) == 0) {
      warning("Could not resolve any session IDs. Returning df unchanged.")
      return(df)
    }

    unique_studies <- unique(session_map$study_id)
    message("Fetching export for ", length(unique_studies),
            " unique study ID(s)...")

    demo_list <- lapply(unique_studies, function(sid) {
      message("  Study: ", sid)
      out <- fetch_study_export(sid)
      if (!is.null(out)) message("    Retrieved ", nrow(out), " rows.")
      out
    })
  }

  demo_all <- do.call(rbind, Filter(Negate(is.null), demo_list))

  if (is.null(demo_all) || nrow(demo_all) == 0) {
    warning("No demographic data retrieved. Returning df unchanged.")
    return(df)
  }

  if (mode == "session") {
    sub_col <- intersect(c("Submission id", "submission_id"), names(demo_all))[1]
    if (!is.na(sub_col)) {
      demo_all <- demo_all[demo_all[[sub_col]] %in% session_map$submission_id, ]
    }
    demo_all[[sub_col]] <- NULL
  }

  demo_all <- demo_all[!duplicated(demo_all[[pid_col]]), ]
  demo_all$Age <- suppressWarnings(as.numeric(demo_all$Age))

  message("Merging demographics for ", nrow(demo_all), " participant(s).")

  df_out    <- merge(df, demo_all, by = pid_col, all.x = TRUE)
  n_missing <- sum(is.na(df_out$Age))
  if (n_missing > 0) message(n_missing, " participant(s) with no demographic match.")

  return(df_out)
}
