#' Build dyadic data (male–female) from long records, with optional APIM format
#'
#' @description
#' Converts long-format couple data (one row per person per within-person
#' combination) into either:
#'
#' - **Dyad-wide** format (`output = "dyad_wide"`): one row per
#'   `(couple_id, time_var, within_vars...)` with paired columns suffixed
#'   `_male` and `_female`.
#' - **APIM double-entry** (`output = "apim_long"`): two rows per dyad/time/within,
#'   where each row treats one partner as the **actor** and the other as the
#'   **partner**, producing columns `actor_<var>` and `partner_<var>`, plus
#'   `actor_id`, `partner_id`, and a `role` indicator (`"male"`/`"female"`).
#'
#' @param data A data frame (or tibble) in long format with one row per person
#'   per within-person combination (e.g., `Time` × `Strategy`).
#' @param couple_id String. Name of the dyad identifier column (default `"Couple_ID"`).
#' @param person_id String. Name of the person identifier column (default `"Prolific_ID"`).
#' @param time_var String. Name of the time variable column (default `"Time"`).
#' @param within_vars Character vector of additional within-person factor names
#'   to retain at the dyadic level (e.g., `"Strategy"`). Use `character(0)` to
#'   collapse across all within-person factors given by `agg`.
#' @param gender_var String. Name of the gender/sex column used to label
#'   partners as male/female (default `"T1A_Demo_Gender"`).
#' @param male_code,female_code Scalars indicating which values of `gender_var`
#'   correspond to male and female (defaults `1` and `2` respectively).
#' @param measure_vars Character vector of measure columns to split/duplicate.
#'   If `NULL` (default), includes all columns not in `{couple_id, person_id,
#'   time_var, within_vars, gender_var}`.
#' @param agg A function to reduce multiple rows within the same
#'   `(person × time × within)` cell (default `dplyr::first`). Common choices:
#'   `mean`, `median`, etc.
#' @param agg_na_rm Logical. If `TRUE`, and if `agg` supports `na.rm`, pass
#'   `na.rm = TRUE` during aggregation (default `TRUE`).
#' @param require_mf Logical. If `TRUE` (default), drop dyads that are not
#'   mixed-sex (same-sex or ambiguous).
#' @param require_complete Logical. If `TRUE`, keep only rows where both male
#'   and female have non-`NA` values for **all** `measure_vars` (applies to the
#'   dyad-wide stage before APIM construction).
#' @param output One of `c("dyad_wide", "apim_long")`. If `"dyad_wide"` (default),
#'   returns wide columns with `_male`/`_female` suffixes. If `"apim_long"`,
#'   returns the double-entry APIM table with `actor_`/`partner_` columns.
#'
#' @details
#' The function:
#' 1) Labels partner sex using `gender_var` and optionally filters to mixed-sex
#'    dyads (`require_mf = TRUE`).
#' 2) Aggregates duplicated records within each `(couple_id, sex, time_var,
#'    within_vars...)` using `agg`.
#' 3) Produces dyad-wide columns or, if requested, constructs APIM long rows by
#'    mapping male→actor/female→partner and the reverse.
#'
#' Distinguishability is assumed via gender (male/female). If your dyads are
#' indistinguishable, you may need additional coding choices not covered here.
#'
#' @return
#' - If `output = "dyad_wide"`: a tibble with one row per
#'   `(couple_id, time_var, within_vars...)` and columns
#'   `{measure_vars}_male`, `{measure_vars}_female`,
#'   plus `{person_id}_male` / `{person_id}_female`.
#' - If `output = "apim_long"`: a tibble with **two rows** per dyad/time/within,
#'   columns: `actor_id`, `partner_id`, `role`, and for every `v` in
#'   `measure_vars`, `actor_v` and `partner_v`.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' df <- tibble::tibble(
#'   Couple_ID = c(1,1,1,1,2,2),
#'   Prolific_ID = c("a","b","a","b","c","d"),
#'   Time = c(1,1,2,2,1,1),
#'   Strategy = c("DA","DA","DA","DA","DB","DB"),
#'   T1A_Demo_Gender = c(1,2,1,2,1,2),  # 1=male, 2=female
#'   intrinsic = c(4.7, 4.9, 4.2, 4.8, 3.5, 3.7),
#'   extrinsic = c(4.5, 4.6, 4.1, 4.4, 3.2, 3.4)
#' )
#'
#' # Dyad-wide
#' dw <- make_dyads_long(
#'   df,
#'   couple_id    = "Couple_ID",
#'   person_id    = "Prolific_ID",
#'   time_var     = "Time",
#'   within_vars  = "Strategy",
#'   gender_var   = "T1A_Demo_Gender",
#'   male_code    = 1,
#'   female_code  = 2,
#'   measure_vars = c("intrinsic", "extrinsic"),
#'   output       = "dyad_wide"
#' )
#'
#' # APIM double-entry
#' ap <- make_dyads_long(
#'   df,
#'   couple_id    = "Couple_ID",
#'   person_id    = "Prolific_ID",
#'   time_var     = "Time",
#'   within_vars  = "Strategy",
#'   gender_var   = "T1A_Demo_Gender",
#'   male_code    = 1,
#'   female_code  = 2,
#'   measure_vars = c("intrinsic", "extrinsic"),
#'   output       = "apim_long"
#' )
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{tidyr::pivot_wider()} for the dyad-wide pivot
#'   \item APIM resources (e.g., Kenny, Kashy, & Cook) for modeling details
#' }
#'
#' @export
#' @importFrom dplyr mutate filter group_by ungroup summarise first n_distinct arrange across if_all bind_rows select
#' @importFrom tidyr pivot_wider
#'
#'
#'
make_dyads_long <- function(
    data,
    couple_id    = "Couple_ID",
    person_id    = "Prolific_ID",
    time_var     = "Time",
    within_vars  = "Strategy",          # can be character(0)
    gender_var   = "T1A_Demo_Gender",
    male_code    = 1,
    female_code  = 2,
    measure_vars = NULL,                # if NULL, auto-detect
    agg          = dplyr::first,        # how to collapse repeats within (id × time × within)
    agg_na_rm    = TRUE,
    require_mf   = TRUE,                # drop same-sex dyads
    require_complete = FALSE,           # require complete male & female for all measures
    output       = c("dyad_wide", "apim_long")
) {

  output <- match.arg(output)
  within_vars <- as.character(within_vars)
  id_cols <- c(couple_id, person_id, time_var, within_vars, gender_var)

  if (is.null(measure_vars)) {
    measure_vars <- setdiff(names(data), id_cols)
  }

  # label sex
  dat <- data %>%
    mutate(
      sex_label = case_when(
        .data[[gender_var]] == male_code   ~ "male",
        .data[[gender_var]] == female_code ~ "female",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(sex_label))

  # optionally drop non-mixed dyads
  if (require_mf) {
    dat <- dat %>%
      group_by(.data[[couple_id]]) %>%
      filter(n_distinct(sex_label) == 2) %>%
      ungroup()
  }

  # helper agg that passes na.rm if available
  agg_fun <- function(x) {
    if ("na.rm" %in% names(formals(agg))) agg(x, na.rm = agg_na_rm) else agg(x)
  }

  # collapse to one row per (couple, sex, time, within...)
  group_cols <- c(couple_id, "sex_label", time_var, within_vars)

  collapsed <- dat %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      !!person_id := dplyr::first(.data[[person_id]]),
      across(all_of(measure_vars), agg_fun),
      .groups = "drop"
    )

  # pivot to dyad-wide (…_male, …_female)
  dyad_wide <- collapsed %>%
    pivot_wider(
      names_from  = sex_label,
      values_from = c(all_of(person_id), all_of(measure_vars)),
      names_glue  = "{.value}_{sex_label}"
    ) %>%
    arrange(across(all_of(c(couple_id, time_var, within_vars))))

  # enforce completeness if requested
  if (require_complete) {
    need_m  <- paste0(measure_vars, "_male")
    need_f  <- paste0(measure_vars, "_female")
    needed  <- intersect(c(need_m, need_f), names(dyad_wide))
    if (length(needed)) {
      dyad_wide <- dyad_wide %>% filter(if_all(all_of(needed), ~ !is.na(.x)))
    }
  }

  if (output == "dyad_wide") return(dyad_wide)

  # ---- APIM double-entry (no .data in non-masked contexts) ----
  id_cols_keep <- c(couple_id, time_var, within_vars)

  make_apim_side <- function(df, actor_role = c("male","female")) {
    actor_role   <- match.arg(actor_role)
    partner_role <- if (actor_role == "male") "female" else "male"

    actor_id_col    <- paste0(person_id, "_", actor_role)
    partner_id_col  <- paste0(person_id, "_", partner_role)
    actor_meas_cols <- paste0(measure_vars, "_", actor_role)
    partn_meas_cols <- paste0(measure_vars, "_", partner_role)

    out <- df %>%
      select(all_of(id_cols_keep),
             all_of(actor_id_col), all_of(partner_id_col),
             all_of(actor_meas_cols), all_of(partn_meas_cols))

    # rename safely using base name mapping (no data mask needed)
    nm <- names(out)
    nm[nm == actor_id_col]   <- "actor_id"
    nm[nm == partner_id_col] <- "partner_id"
    nm[match(actor_meas_cols, nm)]   <- paste0("actor_", measure_vars)
    nm[match(partn_meas_cols, nm)]   <- paste0("partner_", measure_vars)
    names(out) <- nm

    out$role <- actor_role
    out
  }

  apim_long <- bind_rows(
    make_apim_side(dyad_wide, "male"),
    make_apim_side(dyad_wide, "female")
  ) %>%
    arrange(across(all_of(c(couple_id, time_var, within_vars, "role"))))

  apim_long
}
