# MetaForgeR - Experiment Data Downloader
# --------------------------------------------------------------------------
# Auto-load requirements
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
library(httr)

# Base URL
METAFORGE_API_BASE <- "https://us-central1-protocol-js.cloudfunctions.net"

#' Set MetaForge API Token
#' @export
metaforge_set_token <- function(token) {
  options(metaforge_api_token = token)
  invisible(token)
}

#' Get stored MetaForge API Token
#' @keywords internal
metaforge_get_token <- function() {
  token <- getOption("metaforge_api_token")
  if (is.null(token)) {
    stop("API token not set. Use metaforge_set_token('your-token') first.")
  }
  return(token)
}

#' List MetaForge Projects
#' @export
metaforge_list_projects <- function(token = NULL) {
  if (is.null(token)) token <- metaforge_get_token()

  url <- paste0(METAFORGE_API_BASE, "/apiListProjects")

  response <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", token)),
    httr::content_type_json()
  )

  if (httr::status_code(response) != 200) {
    stop("API Error (", httr::status_code(response), ")")
  }

  result <- httr::content(response, "parsed")

  if (length(result$projects) == 0) {
    message("No projects found.")
    return(data.frame(id = character(), title = character()))
  }

  # Standardized extraction without the %||% operator
  projects_list <- lapply(result$projects, function(p) {
    data.frame(
      id = if (is.null(p$id)) NA else p$id,
      title = if (is.null(p$title)) NA else p$title,
      createdAt = if (is.null(p$createdAt)) NA else p$createdAt,
      stringsAsFactors = FALSE
    )
  })

  return(do.call(rbind, projects_list))
}

#' Download MetaForge Project Data
#' @export
metaforge_get_data <- function(project_id, token = NULL) {
  if (is.null(token)) token <- metaforge_get_token()
  if (missing(project_id)) stop("project_id is required.")

  url <- paste0(METAFORGE_API_BASE, "/apiGetProjectData")

  response <- httr::GET(
    url,
    query = list(projectId = project_id, format = "csv"),
    httr::add_headers(Authorization = paste("Bearer", token))
  )

  if (httr::status_code(response) != 200) {
    stop("API Error (", httr::status_code(response), ")")
  }

  content_type <- httr::headers(response)[["content-type"]]

  # Handle CSV response
  if (grepl("text/csv", content_type, fixed = TRUE)) {
    csv_text <- httr::content(response, "text", encoding = "UTF-8")
    if (nchar(trimws(csv_text)) == 0) return(data.frame())

    data_df <- utils::read.csv(text = csv_text, stringsAsFactors = FALSE)
    return(data_df)
  }

  # Fallback for JSON response
  result <- httr::content(response, "parsed")
  if (length(result$data) == 0) return(data.frame())

  data_df <- do.call(rbind, lapply(result$data, function(row) {
    as.data.frame(lapply(row, function(x) if (is.null(x)) NA else x), stringsAsFactors = FALSE)
  }))

  return(data_df)
}
