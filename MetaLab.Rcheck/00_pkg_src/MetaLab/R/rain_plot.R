#' Raincloud-style plots for between-, within-, and mixed-design data
#'
#' @description
#' `rain_plot()` creates raincloud-style plots (violin + raw points + mean ± SD)
#' for between-, within-, or mixed factorial designs. It:
#' \itemize{
#'   \item aggregates to subject-level means for within and mixed designs to avoid pseudo-replication,
#'   \item supports multiple within- and between-subjects factors,
#'   \item visualises means and standard deviations for each cell.
#' }
#'
#' When multiple factors are supplied:
#' \itemize{
#'   \item all within-subject factors are combined into a single x-axis factor using \code{interaction()},
#'   \item the first between-subject factor is mapped to colour/fill (and used for dodging in mixed designs),
#'   \item any additional between-subject factors are combined and used for faceting (columns).
#' }
#'
#' @param data A `data.frame` containing the raw data.
#' @param dv A string giving the name of the numeric dependent variable column in \code{data}.
#' @param id A string giving the name of the participant ID column in \code{data}. Required for
#'   within-subject and mixed designs (i.e., whenever \code{within} is non-empty).
#' @param within An optional character vector of within-subject factor names in \code{data}.
#'   If non-empty, the design is treated as within- or mixed-design. Within-subject factors are
#'   combined into a single factor used on the x-axis via \code{interaction()}.
#' @param between An optional character vector of between-subject factor names in \code{data}.
#'   If non-empty, the design is treated as between- or mixed-design. The first factor is used for
#'   colour/fill (and dodging in mixed designs); any additional factors are combined into a single
#'   facet variable.
#' @param palette A string giving the name of a ColorBrewer palette passed to
#'   \code{\link[ggplot2]{scale_fill_brewer}} and \code{\link[ggplot2]{scale_color_brewer}}.
#'
#' @details
#' The function automatically determines the design type:
#' \itemize{
#'   \item \strong{between-subjects}: \code{length(within) == 0} and \code{length(between) > 0}
#'   \item \strong{within-subjects}:  \code{length(within) > 0} and \code{length(between) == 0}
#'   \item \strong{mixed-design}:     \code{length(within) > 0} and \code{length(between) > 0}
#' }
#'
#' For within and mixed designs, the data are first aggregated to subject-level means within each
#' unique within × between cell before computing cell means and SDs, to ensure that the summary
#' statistics reflect variability between participants rather than repeated trials.
#'
#' @return
#' A \code{\link[ggplot2]{ggplot}} object showing violin densities (the "cloud"),
#' raw jittered data points (the "rain"), and cell means with ±1 SD error bars.
#'
#' @examples
#' \dontrun{
#' # Simple between-subjects design
#' rain_plot(
#'   data    = mydata,
#'   dv      = "accuracy",
#'   between = "group"
#' )
#'
#' # Within-subjects design (e.g., task is repeated within participants)
#' rain_plot(
#'   data   = mydata,
#'   dv     = "confidence",
#'   id     = "participant",
#'   within = "task"
#' )
#'
#' # Mixed design: within (condition) × between (group)
#' rain_plot(
#'   data    = mydata,
#'   dv      = "rt",
#'   id      = "participant",
#'   within  = "condition",
#'   between = "group"
#' )
#'
#' # Multiple between factors: facet by rating_condition
#' rain_plot(
#'   data    = mydata,
#'   dv      = "efficacy_rating",
#'   id      = "participant",
#'   between = c("outcome_condition", "rating_condition")
#' )
#'
#' # Multiple within factors: task × time collapsed onto the x-axis
#' rain_plot(
#'   data   = mydata,
#'   dv     = "confidence",
#'   id     = "participant",
#'   within = c("task", "time")
#' )
#' }
#'
#' @importFrom dplyr group_by summarise mutate across
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot aes geom_violin geom_errorbar geom_point facet_grid
#' @importFrom ggplot2 scale_fill_brewer scale_color_brewer theme_minimal ylab
#' @importFrom ggplot2 element_blank
#' @importFrom ggbeeswarm geom_quasirandom
#' @importFrom rlang sym syms
#'
#' @export
rain_plot <- function(
    data,
    dv,                 # numeric DV (string)
    id = NULL,          # participant ID (string) – required for within/mixed
    within = NULL,      # within-subject factor(s) (character vector)
    between = NULL,     # between-subject factor(s) (character vector)
    palette = "Dark2"
) {
  # Coerce to character vectors (or empty)
  if (is.null(within))  within  <- character(0)
  if (is.null(between)) between <- character(0)

  # Determine design type
  design <- dplyr::case_when(
    length(within)  > 0 & length(between) > 0 ~ "mixed",
    length(within)  > 0                       ~ "within",
    length(between) > 0                       ~ "between",
    TRUE                                      ~ "none"
  )
  if (design == "none") {
    stop("You must specify at least one of 'within' or 'between'.")
  }

  # Symbols
  dv_sym       <- rlang::sym(dv)
  if (!is.null(id)) id_sym <- rlang::sym(id)
  within_syms  <- rlang::syms(within)
  between_syms <- rlang::syms(between)

  df <- data

  # Helper to add combined within-factor column
  add_within_factor <- function(df) {
    if (length(within) == 0) return(df)
    df %>%
      dplyr::mutate(
        .within = interaction(!!!within_syms, sep = " : ", drop = TRUE)
      )
  }

  # Helper to add combined facet column for extra between factors
  add_between_facet <- function(df) {
    if (length(between) <= 1) return(df)
    extra_syms <- rlang::syms(between[-1])
    df %>%
      dplyr::mutate(
        .facet_between = interaction(!!!extra_syms, sep = " : ", drop = TRUE)
      )
  }

  # Build data_plot and sumdat depending on design
  if (design == "between") {

    if (length(between) == 0) {
      stop("For a between-subject design, provide at least one 'between' factor.")
    }

    # No subject aggregation needed
    data_plot <- df %>% add_between_facet()

    # Summary stats per full between combination
    sumdat <- data_plot %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(between))) %>%
      dplyr::summarise(
        mean = mean(!!dv_sym, na.rm = TRUE),
        sd   = stats::sd(!!dv_sym, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_between_facet()

    x_var   <- between[1]
    x_sym   <- rlang::sym(x_var)
    col_sym <- rlang::sym(between[1])

    p <- ggplot2::ggplot(
      data_plot,
      ggplot2::aes(x = !!x_sym, y = !!dv_sym, fill = !!col_sym, colour = !!col_sym)
    ) +
      ggplot2::geom_violin(
        width  = 0.6,
        alpha  = 0.4,
        trim   = FALSE,
        colour = NA
      ) +
      ggbeeswarm::geom_quasirandom(
        width = 0.15,
        alpha = 0.7,
        size  = 1.8
      ) +
      ggplot2::geom_errorbar(
        data = sumdat,
        ggplot2::aes(
          x    = !!x_sym,
          y    = mean,
          ymin = mean - sd,
          ymax = mean + sd
        ),
        width     = 0.08,
        colour    = "black",
        linewidth = 0.7
      ) +
      ggplot2::geom_point(
        data = sumdat,
        ggplot2::aes(x = !!x_sym, y = mean),
        size   = 3,
        colour = "black"
      )

    # Facet on extra between factors (if any)
    if (length(between) > 1) {
      p <- p + ggplot2::facet_grid(. ~ .facet_between)
    }

  } else if (design == "within") {

    if (is.null(id) || length(within) == 0) {
      stop("For a within-subject design, specify both 'id' and at least one 'within' factor.")
    }

    # Aggregate to subject-level means per within cell
    data_plot <- df %>%
      dplyr::group_by(!!id_sym, dplyr::across(dplyr::all_of(within))) %>%
      dplyr::summarise(
        !!dv_sym := mean(!!dv_sym, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      add_within_factor()

    sumdat <- data_plot %>%
      dplyr::group_by(.within) %>%
      dplyr::summarise(
        mean = mean(!!dv_sym, na.rm = TRUE),
        sd   = stats::sd(!!dv_sym, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot2::ggplot(
      data_plot,
      ggplot2::aes(x = .within, y = !!dv_sym, fill = .within, colour = .within)
    ) +
      ggplot2::geom_violin(
        width  = 0.6,
        alpha  = 0.4,
        trim   = FALSE,
        colour = NA
      ) +
      ggbeeswarm::geom_quasirandom(
        width = 0.15,
        alpha = 0.7,
        size  = 1.8
      ) +
      ggplot2::geom_errorbar(
        data = sumdat,
        ggplot2::aes(
          x    = .within,
          y    = mean,
          ymin = mean - sd,
          ymax = mean + sd
        ),
        width     = 0.08,
        colour    = "black",
        linewidth = 0.7
      ) +
      ggplot2::geom_point(
        data = sumdat,
        ggplot2::aes(x = .within, y = mean),
        size   = 3,
        colour = "black"
      )

  } else if (design == "mixed") {

    if (is.null(id) || length(within) == 0 || length(between) == 0) {
      stop("For a mixed design, specify 'id', at least one 'within' and one 'between' factor.")
    }

    # Subject-level means per within × between cell
    data_plot <- df %>%
      dplyr::group_by(
        !!id_sym,
        dplyr::across(dplyr::all_of(within)),
        dplyr::across(dplyr::all_of(between))
      ) %>%
      dplyr::summarise(
        !!dv_sym := mean(!!dv_sym, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      add_within_factor() %>%
      add_between_facet()

    sumdat <- data_plot %>%
      dplyr::group_by(.within, dplyr::across(dplyr::all_of(between))) %>%
      dplyr::summarise(
        mean = mean(!!dv_sym, na.rm = TRUE),
        sd   = stats::sd(!!dv_sym, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_between_facet()

    dodge   <- ggplot2::position_dodge(width = 0.6)
    col_sym <- rlang::sym(between[1])

    p <- ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x      = .within,
        y      = !!dv_sym,
        fill   = !!col_sym,
        colour = !!col_sym
      )
    ) +
      ggplot2::geom_violin(
        position = dodge,
        width    = 0.6,
        alpha    = 0.4,
        trim     = FALSE,
        colour   = NA
      ) +
      ggbeeswarm::geom_quasirandom(
        ggplot2::aes(group = !!col_sym),
        dodge.width = 0.6,
        width       = 0.15,
        alpha       = 0.7,
        size        = 1.8
      ) +
      ggplot2::geom_errorbar(
        data = sumdat,
        ggplot2::aes(
          x      = .within,
          y      = mean,
          ymin   = mean - sd,
          ymax   = mean + sd,
          group  = !!col_sym,
          colour = !!col_sym
        ),
        position  = dodge,
        width     = 0.08,
        linewidth = 0.7
      ) +
      ggplot2::geom_point(
        data = sumdat,
        ggplot2::aes(
          x      = .within,
          y      = mean,
          group  = !!col_sym,
          colour = !!col_sym
        ),
        position = dodge,
        size     = 3
      )

    # Facet on any extra between factors
    if (length(between) > 1) {
      p <- p + ggplot2::facet_grid(. ~ .facet_between)
    }
  }

  # Common theming / scales
  p <- p +
    ggplot2::scale_fill_brewer(palette = palette) +
    ggplot2::scale_color_brewer(palette = palette) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    ) +
    ggplot2::ylab(dv)

  return(p)
}
