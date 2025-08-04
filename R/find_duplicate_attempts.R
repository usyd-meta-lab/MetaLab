#' Identify participants who uploaded data in more than one source file
#'
#' @description
#' Flags “repeat” or *duplicate* attempts in multi‑file datasets by locating
#' any `participant_id` associated with more than one distinct `source_file`.
#' The helper prints a compact summary (or a confirmation message if none are
#' found) and invisibly returns the same information as a tibble for further
#' processing.
#'
#' @param data A data frame (or tibble) containing at least two columns:
#'   one identifying participants and one identifying the file each record
#'   came from.
#' @param id_col Bare (unquoted) column name that uniquely identifies each
#'   participant.  Defaults to `participant_id`.
#' @param file_col Bare (unquoted) column name holding the file identifiers
#'   (e.g. file path, file name, upload ID…).  Defaults to `source_file`.
#' @param quiet Logical.  If `FALSE` (default) the summary or confirmation
#'   message is printed; if `TRUE` the function returns silently.
#'
#' @return
#' A tibble with one row per participant who appears in **more than one**
#' unique file, containing:
#'
#' * `participant_id` – the ID (same name as `id_col`);
#' * `n_files`        – number of different files detected; and
#' * `files`          – a list column with the vector of file names/IDs.
#'
#' The tibble is returned **invisibly** so the printout is optional.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # toy data --------------------------------------------------------
#' trial_data <- tibble(
#'   participant_id = c("P1", "P1", "P2", "P3", "P3", "P3"),
#'   source_file    = c("fileA.csv", "fileB.csv", "fileA.csv",
#'                      "fileC.csv", "fileC.csv", "fileD.csv"),
#'   rt             = runif(6)  # other columns irrelevant
#' )
#'
#' # find repeat submissions ----------------------------------------
#' find_duplicate_attempts(trial_data)
#'
#' # capture silently and filter the main dataset -------------------
#' dups <- find_duplicate_attempts(trial_data, quiet = TRUE)
#' clean_data <- anti_join(trial_data, dups, by = "participant_id")
#' }
#'
#' @seealso [dplyr] for the verbs used under the hood.
#'
#' @export
find_duplicate_attempts <- function(data,
                                    id_col   = participant_id,
                                    file_col = source_file,
                                    quiet    = FALSE) {
  dup_tbl <- data %>%
    dplyr::distinct({{ id_col }}, {{ file_col }}) %>%
    dplyr::group_by({{ id_col }}) %>%
    dplyr::summarise(
      n_files = dplyr::n_distinct({{ file_col }}),
      files   = list(unique({{ file_col }})),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_files > 1)

  if (!quiet) {
    if (nrow(dup_tbl) == 0) {
      message("✅  No duplicate attempts found.")
    } else {
      message("⚠️  Participants with multiple source files (run find_duplicate_attempts function for more details):")
      print(dup_tbl)
    }
  }
  invisible(dup_tbl)
}
